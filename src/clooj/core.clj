; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter MouseAdapter
                           WindowAdapter KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.util Map)
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory)	
           (org.fife.ui.rtextarea RTextScrollPane))
  (:use [clojure.pprint :only (pprint)]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.help :only (arglist-from-caret-pos show-tab-help setup-tab-help
                           setup-completion-list help-handle-caret-move
                           find-focused-text-pane  
                           token-from-caret-pos)]
        [clooj.project :only (add-project load-tree-selection
                              load-expanded-paths load-project-set
                              save-expanded-paths
                              save-tree-selection get-temp-file
                              get-selected-projects
                              get-selected-file-path
                              remove-selected-project update-project-tree
                              rename-project set-tree-selection
                              get-code-files get-selected-namespace)]
        [clooj.utils :only (clooj-prefs write-value-to-prefs read-value-from-prefs
                            is-mac add-text-change-listener
                            set-selection scroll-to-pos add-caret-listener
                            attach-child-action-keys attach-action-keys
                            get-caret-coords add-menu
                            add-menu-item
                            choose-file choose-directory
                            comment-out uncomment-out
                            indent unindent awt-event persist-window-shape
                            confirmed? create-button is-win
                            get-keystroke printstream-to-writer
                            focus-in-text-component
                            scroll-to-caret when-lets
                            constrain-to-parent make-split-pane
                            gen-map on-click
                            remove-text-change-listeners get-text-str 
                            scroll-to-line get-directories)]
        [clooj.indent :only (setup-autoindent fix-indent-selected-lines)]
        [clooj.style :only (get-monospaced-fonts show-font-window)]
        [clooj.navigate :only (attach-navigation-keys)])
  (:require [clojure.main :only (repl repl-prompt)]
            [clojure.set])
  (:gen-class
   :methods [^{:static true} [show [] void]]))


(def gap 5)

(def embedded (atom false))

(def changing-file (atom false))

(defprotocol DynamicWordHighlighter
  (addWordToHighlight [this word token-type]))

(extend-type RSyntaxTextArea
  DynamicWordHighlighter
  (addWordToHighlight [word token-type]))
    
(defn make-rsyntax-text-area []
  (let [tmf (TokenMakerFactory/getDefaultInstance)
        token-maker (.getTokenMaker tmf "text/clojure")
        token-map (.getWordsToHighlight token-maker)
        rsta (proxy [RSyntaxTextArea] []
               (addWordToHighlight [word token-type]
                                   (do
                                     (.put token-map word token-type)
                                     token-type)))]
      (.. rsta getDocument (setTokenMakerFactory tmf))
    rsta))
  
(defn make-text-area [wrap]
  (doto (RSyntaxTextArea.)
    (.setAnimateBracketMatching false)
    (.setBracketMatchingEnabled false)
    (.setAutoIndentEnabled false)
    (.setAntiAliasingEnabled true)
    (.setLineWrap wrap)
    ))

(def get-clooj-version
  (memoize
    (fn []
      (try
        (-> (Thread/currentThread) .getContextClassLoader
            (.getResource "clooj/core.class") .toString
            (.replace "clooj/core.class" "project.clj")
            URL. slurp read-string (nth 2))
        (catch Exception _ nil)))))

;; font

(defonce current-font (atom nil))

(defn font [name size]
  (Font. name Font/PLAIN size))

(def default-font
  (cond (is-mac) ["Monaco" 11]
        (is-win) ["Courier New" 12]
        :else    ["Monospaced" 12]))

(defn set-font
  ([app font-name size]
    (let [f (font font-name size)]
      (awt-event
        (write-value-to-prefs clooj-prefs "app-font"
                              [font-name size])
        (dorun (map #(.setFont (app %) f)
                    [:doc-text-area :repl-in-text-area
                     :repl-out-text-area :arglist-label
                     :search-text-area :help-text-area
                     :completion-list]))
        (reset! current-font [font-name size]))))
  ([app font-name]
    (let [size (second @current-font)]
      (set-font app font-name size))))

(defn load-font [app]
   (apply set-font app (or (read-value-from-prefs clooj-prefs "app-font")
                     default-font)))
  
(defn resize-font [app fun]
  (let [[name size] @current-font]
    (set-font app name (fun size))))

(defn grow-font [app] (resize-font app inc))

(defn shrink-font [app] (resize-font app dec))

;; caret finding

(def highlight-agent (agent nil))

(def arglist-agent (agent nil))

(def caret-position (atom nil))

(defn save-caret-position [app]
  (when-lets [text-area (app :doc-text-area)
              pos (get @caret-position text-area)
              file @(:file app)]
    (when-not (.isDirectory file)
      (let [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))]
        (write-value-to-prefs clooj-prefs key-str pos)))))

(defn load-caret-position [app]
  (when-lets [text-area (app :doc-text-area)
              file @(:file app)]
    (when-not (.isDirectory file)
      (when-lets [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))
                  pos (read-value-from-prefs clooj-prefs key-str)]
        (let [length (.. text-area getDocument getLength)
              pos2 (Math/min pos length)]
          (.setCaretPosition text-area pos2)
          (scroll-to-caret text-area))))))

(defn update-caret-position [text-comp]
  (swap! caret-position assoc text-comp (.getCaretPosition text-comp)))

(defn display-caret-position [app]
  (let [{:keys [row col]} (get-caret-coords (:doc-text-area app))]
    (.setText (:pos-label app) (str " " (inc row) "|" (inc col)))))

(defn handle-caret-move [app text-comp ns]
  (update-caret-position text-comp)
  (help-handle-caret-move app text-comp)
  (let [text (get-text-str text-comp)]
    (send-off highlight-agent
              (fn [old-pos]
                (try
                  (let [pos (@caret-position text-comp)]
                    (when-not (= pos old-pos)
                      (let [enclosing-brackets (find-enclosing-brackets text pos)
                            bad-brackets (find-bad-brackets text)
                            good-enclosures (clojure.set/difference
                                              (set enclosing-brackets) (set bad-brackets))]
                        (awt-event
                          (highlight-brackets text-comp good-enclosures bad-brackets)))))
                  (catch Throwable t (awt-event (.printStackTrace t))))))
    (when ns
      (send-off arglist-agent 
                (fn [old-pos]
                  (try
                    (let [pos (@caret-position text-comp)]
                      (when-not (= pos old-pos)
                        (let [arglist-text
                              (arglist-from-caret-pos app ns text pos)]
                          (awt-event (.setText (:arglist-label app) arglist-text)))))
                    (catch Throwable t (awt-event (.printStackTrace t)))))))))
   
;; highlighting

(defn activate-caret-highlighter [app]
  (when-let [text-comp (app :doc-text-area)]
    (let [f #(handle-caret-move app text-comp (get-file-ns app))]
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f)))
  (when-let [text-comp (app :repl-in-text-area)]
    (let [f #(handle-caret-move app text-comp (get-repl-ns app))]
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f))))

;; double-click paren to select form

(defn double-click-selector [text-comp]
  (.addMouseListener text-comp
    (proxy [MouseAdapter] []
      (mouseClicked [e]
        (when (== 2 (.getClickCount e))
          (when-lets [pos (.viewToModel text-comp (.getPoint e))
                      c (.. text-comp getDocument (getText pos 1) (charAt 0))
                      pos (cond (#{\( \[ \{ \"} c) (inc pos)
                                (#{\) \] \} \"} c) pos)
                      [a b] (find-enclosing-brackets (get-text-str text-comp) pos)]
            (set-selection text-comp a (inc b))))))))

;; temp files

(defn dump-temp-doc [app orig-f txt]
  (try 
    (when orig-f
      (let [orig (.getAbsolutePath orig-f)
            f (.getAbsolutePath (get-temp-file orig-f))]
        (spit f txt)
        (awt-event (.repaint (app :docs-tree)))
        ))
       (catch Exception e nil)))

(def temp-file-manager (agent 0))

(defn update-temp [app]
  (let [text-comp (app :doc-text-area)
        txt (get-text-str text-comp)
        f @(app :file)]
    (send-off temp-file-manager
              (fn [old-pos]
                (try
                  (when-let [pos (get @caret-position text-comp)]
                    (when-not (= old-pos pos)
                      (dump-temp-doc app f txt))
                    pos)
                     (catch Throwable t (awt-event (.printStackTrace t))))))))
  
(defn setup-temp-writer [app]
  (let [text-comp (:doc-text-area app)]
    (add-text-change-listener text-comp
      #(when-not @changing-file
         (update-caret-position text-comp)
         (update-temp app)))))

(declare restart-doc)

(defn file-suffix [^File f]
  (when-lets [name (.getName f)
             last-dot (.lastIndexOf name ".")
             suffix (.substring name (inc last-dot))]
    suffix))
    
(defn text-file? [f]
  (not (some #{(file-suffix f)}
             ["jar" "class" "dll" "jpg" "png" "bmp"])))

(defn setup-tree [app]
  (let [tree (:docs-tree app)
        save #(save-expanded-paths tree)]
    (doto tree
      (.setRootVisible false)
      (.setShowsRootHandles true)
      (.. getSelectionModel (setSelectionMode TreeSelectionModel/SINGLE_TREE_SELECTION))
      (.addTreeExpansionListener
        (reify TreeExpansionListener
          (treeCollapsed [this e] (save))
          (treeExpanded [this e] (save))))
      (.addTreeSelectionListener
        (reify TreeSelectionListener
          (valueChanged [this e]
            (awt-event
              (save-tree-selection tree (.getNewLeadSelectionPath e))
              (let [f (.. e getPath getLastPathComponent
                            getUserObject)]
                (when (and
                        (not= f @(app :file))
                        (text-file? f))
                  (restart-doc app f))))))))))
  
;; build gui

(defn make-scroll-pane [text-area]
  (RTextScrollPane. text-area))

(defn setup-search-text-area [app]
  (let [sta (doto (app :search-text-area)
      (.setVisible false)
      (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY))
      (.addFocusListener (proxy [FocusAdapter] [] (focusLost [_] (stop-find app)))))]
    (add-text-change-listener sta #(update-find-highlight app false))
    (attach-action-keys sta ["ENTER" #(highlight-step app false)]
                            ["shift ENTER" #(highlight-step app true)]
                            ["ESCAPE" #(escape-find app)])))

(defn create-arglist-label []
  (doto (JLabel.)
    (.setVisible true)
    ))

(defn exit-if-closed [^java.awt.Window f]
  (when-not @embedded
    (.addWindowListener f
      (proxy [WindowAdapter] []
        (windowClosing [_]
          (System/exit 0))))))

(def no-project-txt
    "\n Welcome to clooj, a lightweight IDE for clojure\n
     To start coding, you can either\n
       a. create a new project
            (select the Project > New... menu), or
       b. open an existing project
            (select the Project > Open... menu)\n
     and then either\n
       a. create a new file
            (select the File > New menu), or
       b. open an existing file
            (click on it in the tree at left).")
       
(def no-file-txt
    "To edit source code you need to either: <br>
     &nbsp;1. create a new file 
     (select menu <b>File > New...</b>)<br>
     &nbsp;2. edit an existing file by selecting one at left.</html>")


(defn open-project [app]
  (when-let [dir (choose-directory (app :f) "Choose a project directory")]
    (let [project-dir (if (= (.getName dir) "src") (.getParentFile dir) dir)]
      (write-value-to-prefs clooj-prefs "last-open-dir" (.getAbsolutePath (.getParentFile project-dir)))
      (add-project app (.getAbsolutePath project-dir))
      (update-project-tree (:docs-tree app))
      (when-let [clj-file (or (-> (File. project-dir "src")
                                 .getAbsolutePath
                                 (get-code-files ".clj")
                                 first)
                              project-dir)]
        (awt-event (set-tree-selection (app :docs-tree) (.getAbsolutePath clj-file)))))))

(defn attach-global-action-keys [comp app]
  (attach-action-keys comp
    ["cmd1 EQUALS" #(grow-font app)]
    ["cmd1 shift EQUALS" #(grow-font app)]
    ["cmd1 PLUS" #(grow-font app)]                  
    ["cmd2 MINUS" #(.toBack (:frame app))]
    ["cmd2 PLUS" #(.toFront (:frame app))]
    ["cmd2 EQUALS" #(.toFront (:frame app))]
    ["cmd1 shift O" #(open-project app)]
    ["cmd1 K"#(.setText (app :repl-out-text-area) "")]))
  
(defn on-window-activation [win fun]
  (.addWindowListener win
    (proxy [WindowAdapter] []
      (windowActivated [_]
        (fun)))))

(defn create-app []
  (let [doc-text-area (make-text-area false)
        doc-text-panel (JPanel.)
        doc-label (JLabel. "Source Editor")
        repl-out-text-area (make-text-area true)
        repl-out-writer (make-repl-writer repl-out-text-area)
        repl-in-text-area (make-text-area false)
        help-text-area (make-text-area true)
        help-text-scroll-pane (JScrollPane. help-text-area)
        completion-panel (JPanel.)
        completion-label (JLabel. "Name search")
        completion-list (JList.)
        completion-scroll-pane (JScrollPane. completion-list)
        search-text-area (JTextField.)
        arglist-label (create-arglist-label)
        pos-label (JLabel.)
        frame (JFrame.)
        cp (.getContentPane frame)
        layout (SpringLayout.)
        docs-tree (JTree.)
        docs-tree-scroll-pane (JScrollPane. docs-tree)
        docs-tree-panel (JPanel.)
        docs-tree-label (JLabel. "Projects")
        doc-split-pane (make-split-pane
                         docs-tree-panel
                         doc-text-panel true gap 0.25)
        repl-out-scroll-pane (JScrollPane. repl-out-text-area)
        repl-split-pane (make-split-pane
                          repl-out-scroll-pane
                          (make-scroll-pane repl-in-text-area) false gap 0.75)
        repl-panel (JPanel.)
        repl-label (JLabel. "Clojure REPL output")
        repl-input-label (JLabel. "Clojure REPL input \u2191")
        split-pane (make-split-pane doc-split-pane repl-panel true gap 0.5)
        app (merge {:file (atom nil)
                    :repl (atom (create-outside-repl repl-out-writer nil "user"))
                    :changed false}
                   (gen-map
                     doc-text-area
                     doc-label
                     repl-out-text-area
                     repl-in-text-area
                     repl-label
                     frame
                     help-text-area
                     help-text-scroll-pane
                     repl-out-scroll-pane
                     docs-tree
                     docs-tree-scroll-pane
                     docs-tree-panel
                     docs-tree-label
                     search-text-area
                     pos-label
                     repl-out-writer
                     doc-split-pane
                     repl-split-pane
                     split-pane
                     arglist-label
                     completion-list
                     completion-scroll-pane
                     completion-panel
                     ))
        doc-scroll-pane (make-scroll-pane doc-text-area)]
    (doto frame
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add split-pane)
      (.setTitle (str "clooj " (get-clooj-version))))
    (doto doc-text-panel
      (.setLayout (SpringLayout.))
      (.add doc-scroll-pane)
      (.add doc-label)
      (.add pos-label)
      (.add search-text-area)
      (.add arglist-label))
    (doto docs-tree-panel
      (.setLayout (SpringLayout.))
      (.add docs-tree-label)
      (.add docs-tree-scroll-pane))
    (doto repl-panel
      (.setLayout (SpringLayout.))
      (.add repl-label)
      (.add repl-input-label)
      (.add repl-split-pane))
    (doto completion-panel
      (.setLayout (SpringLayout.))
      (.add completion-label)
      (.add completion-scroll-pane))
    (doto doc-text-area
      attach-navigation-keys)
    (constrain-to-parent completion-label :n 0 :w 0 :n 15 :e 0)
    (constrain-to-parent completion-scroll-pane :n 16 :w 0 :s 0 :e 0)
    (constrain-to-parent repl-label :n 0 :w 0 :n 15 :e 0)
    (constrain-to-parent repl-input-label :s -15 :w 0 :s 0 :e 0)
    (constrain-to-parent repl-split-pane :n 16 :w 0 :s -16 :e 0)
    (constrain-to-parent docs-tree-label :n 0 :w 0 :n 15 :e 0)
    (constrain-to-parent docs-tree-scroll-pane :n 16 :w 0 :s 0 :e 0)
    (setup-completion-list completion-list app)
    (doto pos-label
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (double-click-selector doc-text-area)
    (doto repl-in-text-area
      double-click-selector
      attach-navigation-keys)
    (.setSyntaxEditingStyle repl-in-text-area
                            SyntaxConstants/SYNTAX_STYLE_CLOJURE)
    (.setModel docs-tree (DefaultTreeModel. nil))
    (constrain-to-parent split-pane :n gap :w gap :s (- gap) :e (- gap))
    (constrain-to-parent doc-label :n 0 :w 0 :n 15 :e 0)
    (constrain-to-parent doc-scroll-pane :n 16 :w 0 :s -16 :e 0)
    (constrain-to-parent pos-label :s -14 :w 0 :s 0 :w 100)
    (constrain-to-parent search-text-area :s -15 :w 80 :s 0 :w 300)
    (constrain-to-parent arglist-label :s -14 :w 80 :s -1 :e -10)
    (.layoutContainer layout frame)
    (exit-if-closed frame)
    (setup-search-text-area app)
    (add-caret-listener doc-text-area #(display-caret-position app))
    (activate-caret-highlighter app)
    (setup-temp-writer app)
    (attach-action-keys doc-text-area
      ["cmd1 ENTER" #(send-selected-to-repl app)])
    (doto repl-out-text-area (.setEditable false))
    (doto help-text-area (.setEditable false)
                         (.setBackground (Color. 0xFF 0xFF 0xE8)))
    (setup-autoindent repl-in-text-area)
    (setup-tab-help app doc-text-area)
    (dorun (map #(attach-global-action-keys % app)
                [docs-tree doc-text-area repl-in-text-area repl-out-text-area (.getContentPane frame)]))
    (setup-autoindent doc-text-area)
    app))

;; clooj docs

(defn restart-doc [app ^File file]
  (let [f @(:file app)
        txt (get-text-str (:doc-text-area app))]
    (send-off temp-file-manager
              (let [temp-file (get-temp-file f)]
                (fn [_] (when (and f temp-file (.exists temp-file))
                          (dump-temp-doc app f txt))
                  0))))
  (await temp-file-manager)
  (let [frame (app :frame)
        text-area (app :doc-text-area)
        temp-file (get-temp-file file)
        file-to-open (if (and temp-file (.exists temp-file)) temp-file file)
        doc-label (app :doc-label)]
    ;(remove-text-change-listeners text-area)
    (reset! changing-file true)
    (save-caret-position app)
    (.. text-area getHighlighter removeAllHighlights)
    (if (and file-to-open (.exists file-to-open) (.isFile file-to-open))
      (do (let [txt (slurp file-to-open)
                rdr (StringReader. txt)]
            (.read text-area rdr nil))
          (.discardAllEdits text-area)
          (.setText doc-label (str "Source Editor \u2014 " (.getPath file)))
          (.setEditable text-area true)	
          (.setSyntaxEditingStyle text-area
                                  (let [file-name (.getName file-to-open)]
              (if (or (.endsWith file-name ".clj")
                      (.endsWith file-name ".clj~"))
                SyntaxConstants/SYNTAX_STYLE_CLOJURE
                SyntaxConstants/SYNTAX_STYLE_NONE))))
      (do (.setText text-area no-project-txt)
          (.setText doc-label (str "Source Editor (No file selected)"))
          (.setEditable text-area false)))
    (update-caret-position text-area)
    (setup-autoindent text-area)
    (reset! (app :file) file)
    (switch-repl app (first (get-selected-projects app)))
    (apply-namespace-to-repl app)
    (reset! changing-file false)))

(defn save-file [app]
  (try
    (let [f @(app :file)
          ft (File. (str (.getAbsolutePath f) "~"))]
      (with-open [writer (BufferedWriter.
                           (OutputStreamWriter.
                             (FileOutputStream. f)
                             "UTF-8"))]
        (.write (app :doc-text-area) writer))
      (send-off temp-file-manager (fn [_] 0))
      (.delete ft)
      (.repaint (app :docs-tree))
      )
    (catch Exception e (JOptionPane/showMessageDialog
                         nil "Unable to save file."
                         "Oops" JOptionPane/ERROR_MESSAGE))))

(def project-clj-text (.trim
"
(defproject PROJECTNAME \"1.0.0-SNAPSHOT\"
  :description \"FIXME: write\"
  :dependencies [[org.clojure/clojure \"1.3.0\"]])
"))
      
(defn specify-source [project-dir title default-namespace]
  (when-let [namespace (JOptionPane/showInputDialog nil
                         "Please enter a fully-qualified namespace"
                         title
                         JOptionPane/QUESTION_MESSAGE
                         nil
                         nil
                         default-namespace)]
    (let [tokens (map munge (.split namespace "\\."))
          dirs (cons "src" (butlast tokens))
          dirstring (apply str (interpose File/separator dirs))
          name (last tokens)
          the-dir (File. project-dir dirstring)]
      (.mkdirs the-dir)
      [(File. the-dir (str name ".clj")) namespace])))
      
(defn create-file [app project-dir default-namespace]
   (when-let [[file namespace] (specify-source project-dir
                                          "Create a source file"
                                          default-namespace)]
     (let [tree (:docs-tree app)]
       (spit file (str "(ns " namespace ")\n"))
       (update-project-tree (:docs-tree app))
       (set-tree-selection tree (.getAbsolutePath file)))))

(defn new-project-clj [app project-dir]
  (let [project-name (.getName project-dir)
        file-text (.replace project-clj-text "PROJECTNAME" project-name)]
    (spit (File. project-dir "project.clj") file-text)))

(defn new-project [app]
  (try
    (when-let [dir (choose-file (app :frame) "Create a project directory" "" false)]
      (awt-event
        (let [path (.getAbsolutePath dir)]
          (.mkdirs (File. dir "src"))
          (new-project-clj app dir)
          (add-project app path)
          (update-project-tree (:docs-tree app))
          (set-tree-selection (app :docs-tree) path)
          (create-file app dir (str (.getName dir) ".core")))))
      (catch Exception e (do (JOptionPane/showMessageDialog nil
                               "Unable to create project."
                               "Oops" JOptionPane/ERROR_MESSAGE)
                           (.printStackTrace e)))))
  
(defn rename-file [app]
  (when-let [old-file @(app :file)]
    (let [tree (app :docs-tree)
          [file namespace] (specify-source
                             (first (get-selected-projects app))
                             "Rename a source file"
                             (get-selected-namespace tree))]
      (when file
        (.renameTo @(app :file) file)33
        (update-project-tree (:docs-tree app))
        (awt-event (set-tree-selection tree (.getAbsolutePath file)))))))

(defn delete-file [app]
  (let [path (get-selected-file-path app)]
    (when (confirmed? "Are you sure you want to delete this file?\nDeleting cannot be undone." path)
      (loop [f (File. path)]
        (when (and (empty? (.listFiles f))
                   (let [p (-> f .getParentFile .getAbsolutePath)]
                     (or (.contains p (str File/separator "src" File/separator))
                         (.endsWith p (str File/separator "src")))))
          (.delete f)
          (recur (.getParentFile f))))
      (update-project-tree (app :docs-tree)))))

(defn remove-project [app]
  (when (confirmed? "Remove the project from list? (No files will be deleted.)"
                    "Remove project")
    (remove-selected-project app)))

(defn revert-file [app]
  (when-let [f @(:file app)]
    (let [temp-file (get-temp-file f)]
      (when (.exists temp-file))
        (let [path (.getAbsolutePath f)]
          (when (confirmed? "Revert the file? This cannot be undone." path)
            (.delete temp-file)
            (update-project-tree (:docs-tree app))
            (restart-doc app f))))))

(defn- dir-rank [dir]
  (get {"src" 0 "test" 1 "lib" 2} (.getName dir) 100))

(defn- find-file [project-path relative-file-path]
  (let [classpath-dirs (sort-by dir-rank < (get-directories (File. project-path)))
        file-candidates (map 
                          #(File. (str (.getAbsolutePath %) File/separatorChar relative-file-path)) 
                          classpath-dirs)]
    (first (filter #(and (.exists %) (.isFile %)) file-candidates))))

(defn goto-definition [ns app]
  (let [text-comp (:doc-text-area app)
        pos (.getCaretPosition text-comp)
        text (.getText text-comp)
        src-file (:file (meta (do 
                                            (token-from-caret-pos ns text pos) nil)))
        line (:line (meta (do (find-ns (symbol ns))
                                        (token-from-caret-pos ns text pos) nil)))
        project-path (first (get-selected-projects app))
        file (find-file project-path src-file)]
    (when (and file line)
      (when (not= file @(:file app))
        (restart-doc app file)
        (set-tree-selection (:docs-tree app) (.getAbsolutePath file)))
      (scroll-to-line text-comp line))))

(defn make-menus [app]
  (when (is-mac)
    (System/setProperty "apple.laf.useScreenMenuBar" "true"))
  (let [menu-bar (JMenuBar.)]
    (. (app :frame) setJMenuBar menu-bar)
    (let [file-menu
          (add-menu menu-bar "File" "F"
            ["New" "N" "cmd1 N" #(create-file app (first (get-selected-projects app)) "")]
            ["Save" "S" "cmd1 S" #(save-file app)]
            ["Move/Rename" "M" nil #(rename-file app)]
            ["Revert" "R" nil #(revert-file app)]
            ["Delete" nil nil #(delete-file app)])]
      (when-not (is-mac)
        (add-menu-item file-menu "Exit" "X" nil #(System/exit 0))))
    (add-menu menu-bar "Project" "P"
      ["New..." "N" "cmd1 shift N" #(new-project app)]
      ["Open..." "O" "cmd1 shift O" #(open-project app)]
      ["Move/Rename" "M" nil #(rename-project app)]
      ["Remove" nil nil #(remove-project app)])
    (add-menu menu-bar "Source" "U"
      ["Comment-out" "C" "cmd1 SEMICOLON" #(comment-out (:doc-text-area app))]
      ["Uncomment-out" "U" "cmd1 shift SEMICOLON" #(uncomment-out (:doc-text-area app))]
      ["Fix indentation" "F" "cmd1 BACK_SLASH" #(fix-indent-selected-lines (:doc-text-area app))]
      ["Indent lines" "I" "cmd1 CLOSE_BRACKET" #(indent (:doc-text-area app))]
      ["Unindent lines" "D" "cmd1 OPEN_BRACKET" #(unindent (:doc-text-area app))]
      ["Name search/docs" "S" "TAB" #(show-tab-help app (find-focused-text-pane app) inc)]
      ;["Go to definition" "G" "cmd1 D" #(goto-definition (get-file-ns app) app)]
      )
    (add-menu menu-bar "REPL" "R"
      ["Evaluate here" "E" "cmd1 ENTER" #(send-selected-to-repl app)]
      ["Evaluate entire file" "F" "cmd1 E" #(send-doc-to-repl app)]
      ["Apply file ns" "A" "cmd1 shift A" #(apply-namespace-to-repl app)]
      ["Clear output" "C" "cmd1 K" #(.setText (app :repl-out-text-area) "")]
      ["Restart" "R" "cmd1 R" #(restart-repl app
                            (first (get-selected-projects app)))]
      ["Print stack trace for last error" "T" "cmd1 T" #(print-stack-trace app)])
    (add-menu menu-bar "Search" "S"
      ["Find" "F" "cmd1 F" #(start-find app)]
      ["Find next" "N" "cmd1 G" #(highlight-step app false)]
      ["Find prev" "P" "cmd1 shift G" #(highlight-step app true)])
    (add-menu menu-bar "Window" "W"
      ["Go to REPL input" "R" "cmd1 3" #(.requestFocusInWindow (:repl-in-text-area app))]
      ["Go to Editor" "E" "cmd1 2" #(.requestFocusInWindow (:doc-text-area app))]
      ["Go to Project Tree" "P" "cmd1 1" #(.requestFocusInWindow (:docs-tree app))]
      ["Increase font size" nil "cmd1 PLUS" #(grow-font app)]
      ["Decrease font size" nil "cmd1 MINUS" #(shrink-font app)]
      ["Choose font..." nil nil #(apply show-font-window
                                        app set-font @current-font)])))
      
    
(defn add-visibility-shortcut [app]
  (let [shortcuts [(map get-keystroke ["cmd2 EQUALS" "cmd2 PLUS"])]]
    (.. Toolkit getDefaultToolkit
      (addAWTEventListener
        (proxy [AWTEventListener] []
          (eventDispatched [e]
            (when (some #{(KeyStroke/getKeyStrokeForEvent e)}
                     shortcuts)
              (.toFront (:frame app)))))
        AWTEvent/KEY_EVENT_MASK))))

;; startup

(defonce current-app (atom nil))

(defn startup []
  (Thread/setDefaultUncaughtExceptionHandler
    (proxy [Thread$UncaughtExceptionHandler] []
      (uncaughtException [thread exception]
                       (println thread) (.printStackTrace exception))))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [app (create-app)]
    (reset! current-app app)
    (make-menus app)
    (add-visibility-shortcut app)
    (add-repl-input-handler app)
    (setup-tab-help app (app :repl-in-text-area))
    (doall (map #(add-project app %) (load-project-set)))
    (let [frame (app :frame)]
      (persist-window-shape clooj-prefs "main-window" frame) 
      (.setVisible frame true)
      (on-window-activation frame #(update-project-tree (app :docs-tree))))
    (setup-temp-writer app)
    (setup-tree app)
    (let [tree (app :docs-tree)]
      (load-expanded-paths tree)
      (load-tree-selection tree))
    (load-font app)))
(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (startup)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (reset! embedded false)
  (startup))

;; testing

(defn get-text []
  (get-text-str (@current-app :doc-text-area)))

; not working yet:
;(defn restart
;   "Restart the application"
;   []
;  (.setVisible (@current-app :frame) false)
;  (startup))
