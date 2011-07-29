; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.core
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree SpringLayout JTextPane
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.text DocumentFilter)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Point Rectangle Window)
           (java.awt.event FocusAdapter WindowAdapter)
           (java.awt Color Font GridLayout)
           (java.net URL)
           (java.io File FileReader FileWriter))
  (:use [clojure.contrib.duck-streams :only (writer)]
        [clojure.pprint :only (pprint)]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.help :only (arglist-from-caret-pos)]
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
                            is-mac count-while add-text-change-listener
                            set-selection scroll-to-pos add-caret-listener
                            attach-child-action-keys attach-action-keys
                            get-caret-coords add-menu make-undoable
                            choose-file choose-directory
                            comment-out uncomment-out
                            indent unindent awt-event persist-window-shape
                            confirmed? create-button is-win)]
        [clooj.indent :only (setup-autoindent)])
  (:require [clojure.contrib.string :as string]
            [clojure.main :only (repl repl-prompt)])
  (:gen-class
   :methods [^{:static true} [show [] void]]))


(def gap 5)

(def embedded (atom false))
  
(def mono-font
  (cond (is-mac) (Font. "Monaco" Font/PLAIN 11)
        (is-win) (Font. "Courier New" Font/PLAIN 12)
        :else    (Font. "Monospaced" Font/PLAIN 12)))

(defn make-text-area [wrap]
  (doto (proxy [JTextPane] []
          (getScrollableTracksViewportWidth []
            (if-not wrap
              (if-let [parent (.getParent this)]
                (<= (. this getWidth)
                    (. parent getWidth))
                false)
              true)))
    (.setFont mono-font)))  

(def get-clooj-version
  (memoize
    (fn []
      (-> (Thread/currentThread) .getContextClassLoader
          (.getResource "clooj/core.class") .toString
          (.replace "clooj/core.class" "project.clj")
          URL. slurp read-string (nth 2)))))
    
;; caret finding

(def highlight-agent (agent nil))

(def arglist-agent (agent nil))

(def caret-position (atom nil))

(defn update-caret-position [text-comp]
  (swap! caret-position assoc text-comp (.getCaretPosition text-comp)))

(defn display-caret-position [doc]
  (let [{:keys [row col]} (get-caret-coords (:doc-text-area doc))]
    (.setText (:pos-label doc) (str " " (inc row) "|" (inc col)))))

(defn handle-caret-move [doc text-comp ns]
  (update-caret-position text-comp)
  (send-off highlight-agent
    (fn [old-pos]
      (try
        (let [pos (@caret-position text-comp)
              text (.getText text-comp)]
          (when-not (= pos old-pos)
            (let [enclosing-brackets (find-enclosing-brackets text pos)
                  bad-brackets (find-bad-brackets text)
                  good-enclosures (clojure.set/difference
                                    (set enclosing-brackets) (set bad-brackets))]
              (awt-event
                (highlight-brackets text-comp good-enclosures bad-brackets)))))
        (catch Throwable t (.printStackTrace t)))))
  (send-off arglist-agent 
    (fn [old-pos]
      (try
        (let [pos (@caret-position text-comp)
              text (.getText text-comp)]
          (when-not (= pos old-pos)
            (let [arglist-text
                   (arglist-from-caret-pos (find-ns (symbol ns)) text pos)]
              (awt-event (.setText (:arglist-label doc) arglist-text)))))
        (catch Throwable t (.printStackTrace t))))))
  
;; highlighting

(defn activate-caret-highlighter [doc]
  (when-let [text-comp (doc :doc-text-area)]
    (let [f #(handle-caret-move doc text-comp (get-current-namespace text-comp))]
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f)))
  (when-let [text-comp (doc :repl-in-text-area)]
    (let [f  #(handle-caret-move doc text-comp (get-repl-ns doc))]
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f))))

;; temp files

(defn dump-temp-doc [doc orig-f txt]
  (try 
    (when orig-f
      (let [orig (.getAbsolutePath orig-f)
            f (.getAbsolutePath (get-temp-file orig-f))]
         (spit f txt)
         (awt-event (.updateUI (doc :docs-tree)))))
    (catch Exception e nil)))

(def temp-file-manager (agent 0))

(defn update-temp [doc]
  (let [text-comp (doc :doc-text-area)
        txt (.getText text-comp)
        f @(doc :file)]
    (send-off temp-file-manager
      (fn [old-pos]
        (try
          (let [pos (@caret-position text-comp)]
            (when-not (== old-pos pos)
              (dump-temp-doc doc f txt))
            pos)
          (catch Throwable t (.printStackTrace t)))))))
  
(defn setup-temp-writer [doc]
  (let [text-comp (:doc-text-area doc)]
    (add-text-change-listener text-comp
      #(do (update-caret-position text-comp)
           (update-temp doc)))))

(declare restart-doc)

(defn setup-tree [doc]
  (let [tree (:docs-tree doc)
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
                (when (.. f getName (endsWith ".clj"))
                  (restart-doc doc f))))))))))
  
;; build gui

(defn make-scroll-pane [text-area]
  (JScrollPane. text-area))

(defn put-constraint [comp1 edge1 comp2 edge2 dist]
  (let [edges {:n SpringLayout/NORTH
               :w SpringLayout/WEST
               :s SpringLayout/SOUTH
               :e SpringLayout/EAST}]
  (.. comp1 getParent getLayout
            (putConstraint (edges edge1) comp1 
                           dist (edges edge2) comp2))))

(defn put-constraints [comp & args]
  (let [args (partition 3 args)
        edges [:n :w :s :e]]
    (dorun (map #(apply put-constraint comp %1 %2) edges args))))

(defn constrain-to-parent [comp & args]
  (apply put-constraints comp
         (flatten (map #(cons (.getParent comp) %) (partition 2 args)))))

(defn add-line-numbers [text-comp max-lines]
  (let [row-height (.. text-comp getGraphics
                       (getFontMetrics (. text-comp getFont)) getHeight)
        sp (.. text-comp getParent getParent)
        jl (JList.
             (proxy [AbstractListModel] []
               (getSize [] max-lines)
               (getElementAt [i] (str (inc i) " "))))
        cr (. jl getCellRenderer)]
    (.setMargin text-comp (Insets. 0 10 0 0))
    (dorun (map #(.removeMouseListener jl %) (.getMouseListeners jl)))
    (dorun (map #(.removeMouseMotionListener jl %) (.getMouseMotionListeners jl)))
    (doto jl
      (.setBackground (Color. 235 235 235))
      (.setForeground (Color. 50 50 50))
      (.setFixedCellHeight row-height)
      (.setFont (Font. "Monaco" Font/PLAIN 8)))
    (doto cr
      (.setHorizontalAlignment JLabel/RIGHT)
      (.setVerticalAlignment JLabel/BOTTOM))
    (.setRowHeaderView sp jl)))

(defn make-split-pane [comp1 comp2 horizontal resize-weight]
  (doto (JSplitPane. (if horizontal JSplitPane/HORIZONTAL_SPLIT 
                                    JSplitPane/VERTICAL_SPLIT)
                     true comp1 comp2)
        (.setResizeWeight resize-weight)
        (.setOneTouchExpandable false)
        (.setBorder (BorderFactory/createEmptyBorder))
        (.setDividerSize gap)))

(defn setup-search-text-area [doc]
  (let [sta (doto (doc :search-text-area)
      (.setVisible false)
      (.setFont mono-font)
      (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY))
      (.addFocusListener (proxy [FocusAdapter] [] (focusLost [_] (stop-find doc)))))]
    (add-text-change-listener sta #(update-find-highlight doc false))
    (attach-action-keys sta ["ENTER" #(highlight-step doc false)]
                            ["shift ENTER" #(highlight-step doc true)]
                            ["ESCAPE" #(escape-find doc)])))

(defn create-arglist-label []
  (doto (JLabel.)
    (.setVisible true)
    (.setFont mono-font)))

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


(defn open-project [doc]
  (when-let [dir (choose-directory (doc :f) "Choose a project directory")]
    (let [project-dir (if (= (.getName dir) "src") (.getParentFile dir) dir)]
      (add-project doc (.getAbsolutePath project-dir))
      (when-let [clj-file (-> (File. project-dir "src")
                              .getAbsolutePath
                              (get-code-files ".clj")
                              first
                              .getAbsolutePath)]
        (awt-event (set-tree-selection (doc :docs-tree) clj-file))))))

(defn create-doc []
  (let [doc-text-area (make-text-area false)
        doc-text-panel (JPanel.)
        repl-out-text-area (make-text-area true)
        repl-out-writer (make-repl-writer repl-out-text-area)
        repl-in-text-area (make-text-area false)
        search-text-area (JTextField.)
        arglist-label (create-arglist-label)
        pos-label (JLabel.)
        f (JFrame.)
        cp (.getContentPane f)
        layout (SpringLayout.)
        docs-tree (JTree.)
        doc-split-pane (make-split-pane
                         (make-scroll-pane docs-tree)
                         doc-text-panel true 0)
        repl-split-pane (make-split-pane
                          (make-scroll-pane repl-out-text-area)
                          (make-scroll-pane repl-in-text-area) false 0.75)
        split-pane (make-split-pane doc-split-pane repl-split-pane true 0.5)
        doc {:doc-text-area doc-text-area
             :repl-out-text-area repl-out-text-area
             :repl-in-text-area repl-in-text-area
             :frame f
             :docs-tree docs-tree
             :search-text-area search-text-area
             :pos-label pos-label :file (atom nil)
             :repl-out-writer repl-out-writer
             :repl (atom (create-clojure-repl repl-out-writer nil))
             :doc-split-pane doc-split-pane
             :repl-split-pane repl-split-pane
             :split-pane split-pane
             :changed false
             :arglist-label arglist-label}
        doc-scroll-pane (make-scroll-pane doc-text-area)]
    (doto f
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add split-pane))
    (doto doc-text-panel
      (.setLayout (SpringLayout.))
      (.add doc-scroll-pane)
      (.add pos-label)
      (.add search-text-area)
      (.add arglist-label))
    (doto pos-label
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (.setModel docs-tree (DefaultTreeModel. nil))
    (constrain-to-parent split-pane :n gap :w gap :s (- gap) :e (- gap))
    (constrain-to-parent doc-scroll-pane :n 0 :w 0 :s -16 :e 0)
    (constrain-to-parent pos-label :s -14 :w 0 :s 0 :w 100)
    (constrain-to-parent search-text-area :s -15 :w 80 :s 0 :w 300)
    (constrain-to-parent arglist-label :s -14 :w 80 :s -1 :e -10)
    (.layoutContainer layout f)
    (exit-if-closed f)
    (setup-search-text-area doc)
    (add-caret-listener doc-text-area #(display-caret-position doc))
    (activate-caret-highlighter doc)
    (doto repl-out-text-area (.setEditable false))
    (make-undoable repl-in-text-area)
    (setup-autoindent repl-in-text-area)
    (setup-tree doc)
    (attach-action-keys doc-text-area
      ["cmd shift O" #(open-project doc)])
    doc))

;; clooj docs

(defn restart-doc [doc ^File file] 
  (send-off temp-file-manager
    (let [f @(:file doc)
          txt (.getText (:doc-text-area doc))]
      (let [temp-file (get-temp-file f)]
        (fn [_] (when (and f temp-file (.exists temp-file))
                  (dump-temp-doc doc f txt))
                0))))
  (let [frame (doc :frame)]
    (let [text-area (doc :doc-text-area)
          temp-file (get-temp-file file)
          file-to-open (if (and temp-file (.exists temp-file)) temp-file file)
          clooj-name (str "clooj " (get-clooj-version))]
      (.. text-area getHighlighter removeAllHighlights)
      (if (and file-to-open (.exists file-to-open) (.isFile file-to-open))
        (do (with-open [rdr (FileReader. file-to-open)]
              (.read text-area rdr nil))
            (.setTitle frame (str clooj-name " \u2014  " (.getPath file)))
            (.setEditable text-area true))
        (do (.setText text-area no-project-txt)
            (.setTitle frame (str clooj-name " \u2014 (No file selected)"))
            (.setEditable text-area false)))
      (make-undoable text-area)
      (setup-autoindent text-area)
      (reset! (doc :file) file)
      (setup-temp-writer doc)
      (switch-repl doc (first (get-selected-projects doc)))
      (apply-namespace-to-repl doc))
    doc))

(defn new-file [doc]
  (restart-doc doc nil))

(defn save-file [doc]
  (try
    (let [f @(doc :file)
          ft (File. (str (.getAbsolutePath f) "~"))]
      (with-open [writer (FileWriter. f)]
        (.write (doc :doc-text-area) writer))
      (send-off temp-file-manager (fn [_] 0))
      (.delete ft)
      (.updateUI (doc :docs-tree)))
    (catch Exception e (JOptionPane/showMessageDialog
                         nil "Unable to save file."
                         "Oops" JOptionPane/ERROR_MESSAGE))))

(def project-clj-text (.trim
"
(defproject PROJECTNAME \"1.0.0-SNAPSHOT\"
  :description \"FIXME: write\"
  :dependencies [[org.clojure/clojure \"1.2.1\"]
                 [org.clojure/clojure-contrib \"1.2.0\"]])
"))
      
(defn specify-source [project-dir title default-namespace]
  (when-let [namespace (JOptionPane/showInputDialog nil
                         "Please enter a fully-qualified namespace"
                         title
                         JOptionPane/QUESTION_MESSAGE
                         nil
                         nil
                         default-namespace)]
    (let [tokens (.split namespace "\\.")
          dirs (cons "src" (butlast tokens))
          dirstring (apply str (interpose File/separator dirs))
          name (last tokens)
          the-dir (File. project-dir dirstring)]
      (.mkdirs the-dir)
      [(File. the-dir (str name ".clj")) namespace])))
      
(defn create-file [doc project-dir default-namespace]
   (when-let [[file namespace] (specify-source project-dir
                                          "Create a source file"
                                          default-namespace)]
     (let [tree (:docs-tree doc)]
       (spit file (str "(ns " namespace ")\n"))
       (update-project-tree (:docs-tree doc))
       (set-tree-selection tree (.getAbsolutePath file)))))

(defn new-project-clj [doc project-dir]
  (let [project-name (.getName project-dir)
        file-text (.replace project-clj-text "PROJECTNAME" project-name)]
    (spit (File. project-dir "project.clj") file-text)))

(defn new-project [doc]
  (try
    (when-let [dir (choose-file (doc :frame) "Create a project directory" "" false)]
      (awt-event
        (let [path (.getAbsolutePath dir)]
          (.mkdirs (File. dir "src"))
          (new-project-clj doc dir)
          (add-project doc path)
          (set-tree-selection (doc :docs-tree) path)
          (create-file doc dir (str (.getName dir) ".core")))))
      (catch Exception e (do (JOptionPane/showMessageDialog nil
                               "Unable to create project."
                               "Oops" JOptionPane/ERROR_MESSAGE)
                           (.printStackTrace e)))))
  
(defn rename-file [doc]
  (when-let [old-file @(doc :file)]
    (let [tree (doc :docs-tree)
          [file namespace] (specify-source
                             (first (get-selected-projects doc))
                             "Rename a source file"
                             (get-selected-namespace tree))]
      (when file
        (.renameTo @(doc :file) file)
        (update-project-tree (:docs-tree doc))
        (awt-event (set-tree-selection tree (.getAbsolutePath file)))))))

(defn delete-file [doc]
  (let [path (get-selected-file-path doc)]
    (when (confirmed? "Are you sure you want to delete this file?" path)
      (loop [f (File. path)]
        (when (and (empty? (.listFiles f))
                   (let [p (-> f .getParentFile .getAbsolutePath)]
                     (or (.contains p (str File/separator "src" File/separator))
                         (.endsWith p (str File/separator "src")))))
          (.delete f)
          (recur (.getParentFile f))))
      (update-project-tree (doc :docs-tree)))))

(defn remove-project [doc]
  (when (confirmed? "Remove the project from list? (No files will be deleted.)"
                    "Remove project")
    (remove-selected-project doc)))

(defn revert-file [doc]
  (when-let [f @(:file doc)]
    (let [temp-file (get-temp-file f)]
      (when (.exists temp-file))
        (let [path (.getAbsolutePath f)]
          (when (confirmed? "Revert the file? This cannot be undone." path)
            (.delete temp-file)
            (update-project-tree (:docs-tree doc))
            (restart-doc doc f))))))
      
(defn make-menus [doc]
  (when (is-mac)
    (System/setProperty "apple.laf.useScreenMenuBar" "true"))
  (let [menu-bar (JMenuBar.)]
    (. (doc :frame) setJMenuBar menu-bar)
    (add-menu menu-bar "File"
      ["New" "cmd N" #(create-file doc (first (get-selected-projects doc)) "")]
      ["Save" "cmd S" #(save-file doc)]
      ["Move/Rename" nil #(rename-file doc)]
      ["Revert" nil #(revert-file doc)]
      ["Delete" nil #(delete-file doc)])
    (add-menu menu-bar "Project"
      ["New..." "cmd shift N" #(new-project doc)]
      ["Open..." "cmd shift O" #(open-project doc)]
      ["Move/Rename" nil #(rename-project doc)]
      ["Remove" nil #(remove-project doc)])
    (add-menu menu-bar "Source"
      ["Comment-out" "cmd SEMICOLON" #(comment-out (:doc-text-area doc))]
      ["Uncomment-out" "cmd shift SEMICOLON" #(uncomment-out (:doc-text-area doc))]
      ["Indent" "TAB" #(indent (:doc-text-area doc))]
      ["Unindent" "shift TAB" #(unindent (:doc-text-area doc))])
    (add-menu menu-bar "REPL"
      ["Evaluate here" "cmd ENTER" #(send-selected-to-repl doc)]
      ["Evaluate entire file" "cmd E" #(send-doc-to-repl doc)]
      ["Apply file ns" "cmd L" #(apply-namespace-to-repl doc)]
      ["Clear output" "cmd K" #(.setText (doc :repl-out-text-area) "")]
      ["Restart" "cmd R" #(restart-repl doc
                            (first (get-selected-projects doc)))])
    (add-menu menu-bar "Search"
      ["Find" "cmd F" #(start-find doc)]
      ["Find next" "cmd G" #(highlight-step doc false)]
      ["Find prev" "cmd shift G" #(highlight-step doc true)])
    (add-menu menu-bar "Window"
      ["Move to REPL" "alt R" #(.requestFocusInWindow (:repl-in-text-area doc))]
      ["Move to Editor" "alt E" #(.requestFocusInWindow (:doc-text-area doc))]
      ["Move to Project Tree" "alt P" #(.requestFocusInWindow (:docs-tree doc))])))

;; startup

(defonce current-doc (atom nil))

(defn startup []
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [doc (create-doc)]
     (reset! current-doc doc)
     (make-menus doc)
     (let [ta-in (doc :repl-in-text-area)
           ta-out (doc :repl-out-text-area)]
       (add-repl-input-handler doc))
     (doall (map #(add-project doc %) (load-project-set)))
     (persist-window-shape clooj-prefs "main-window" (doc :frame)) 
     (.setVisible (doc :frame) true)
     (add-line-numbers (doc :doc-text-area) Short/MAX_VALUE)
     (setup-temp-writer doc)
     (let [tree (doc :docs-tree)]
       (load-expanded-paths tree)
       (load-tree-selection tree))
     (awt-event
       (let [path (get-selected-file-path doc)
              file (when path (File. path))]
         (restart-doc doc file)))))

(defn -show []
  (reset! embedded true)
  (if (not @current-doc)
    (startup)
    (.setVisible (:frame @current-doc) true)))

(defn -main [& args]
  (reset! embedded false)
  (startup))

;; testing

(defn get-text []
  (.getText (@current-doc :doc-text-area)))

; not working yet:
;(defn restart
;   "Restart the application"
;   []
;  (.setVisible (@current-doc :frame) false)
;  (startup))