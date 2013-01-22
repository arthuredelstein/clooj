; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.project
  (:import (java.io File)
           (java.awt GridLayout)
           (javax.swing JButton JOptionPane JWindow)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel))
  (:use [clooj.utils :only (clooj-prefs read-value-from-prefs
                            write-value-to-prefs awt-event
                            choose-file)]
        [clojure.java.io :only (file)]))

;; projects tree

(declare restart-doc)

(def project-set (atom (sorted-set)))

(defn save-project-set []
  (write-value-to-prefs clooj-prefs "project-set" @project-set))
    
(defn load-project-set []
  (reset! project-set (into (sorted-set)
                            (read-value-from-prefs clooj-prefs "project-set"))))

(defn tree-path-to-file [^TreePath tree-path]
  (when tree-path
    (try (.. tree-path getLastPathComponent getUserObject getAbsolutePath)
         (catch Exception e nil))))

;; loading and saving expanded paths

(defn get-row-path [tree row]
  (tree-path-to-file (. tree getPathForRow row)))

(defn get-expanded-paths [tree]
  (for [i (range (.getRowCount tree)) :when (.isExpanded tree i)]
    (get-row-path tree i)))

(defn save-expanded-paths [tree]
  (write-value-to-prefs clooj-prefs "expanded-paths" (get-expanded-paths tree)))

(defn expand-paths [tree paths]
  (doseq [i (range) :while (< i (.getRowCount tree))]
    (when-let [x (some #{(tree-path-to-file (. tree getPathForRow i))} paths)]
      (.expandPath tree (. tree getPathForRow i)))))

(defn load-expanded-paths [tree]
  (let [paths (read-value-from-prefs clooj-prefs "expanded-paths")]
    (when paths
      (expand-paths tree paths))))

;; loading and saving tree selection

(defn save-tree-selection [tree path]
  (write-value-to-prefs
    clooj-prefs "tree-selection"
    (tree-path-to-file path)))

(defn path-components
  "Generates a sequence of the components in a file path."
  [the-file]
  (->>
    (-> the-file
        file
        .getAbsolutePath
        (.split (str "\\" File/separator)))
    (remove empty?)
    (remove #(= % "."))))
      
(defn file-ancestor?
  "In the file tree, returns true if descendant-file
   is a direct descendant of ancestor-file.
   Also returns true if the files are the same."
  [ancestor-file descendant-file]
  (let [ancestor (path-components ancestor-file)
        descendant (path-components descendant-file)]
    (and (every? true? (map = ancestor descendant))
         (<= (count ancestor) (count descendant)))))
    
(defn node-children [node]
  (when-not (.isLeaf node)
    (for [i (range (.getChildCount node))]
      (.getChildAt node i))))

(defn path-to-node
  "Find the tree node corresponding to a particular file path."
  [tree path]
  (let [root-node (.. tree getModel getRoot)]
    (loop [node root-node]
      (when (and node (not (.isLeaf node)))
        (when-let [children (node-children node)]
                   (let [closer-node (first
                            (filter #(file-ancestor?
                                       (.getUserObject %) path)
                                    children))]
          (when closer-node
            (if (= (file path)
                   (.getUserObject closer-node))
              closer-node
              (recur closer-node)))))))))

(defn row-for-path [tree path]
  (first
    (for [i (range 1 (.getRowCount tree))
          :when (= path
                   (-> tree (.getPathForRow i)
                            .getPath last .getUserObject .getAbsolutePath))]
      i)))

(defn set-tree-selection [tree path]
  (awt-event
    (when-let [node (path-to-node tree path)]
      (let [node-path (.getPath node)
            paths (map #(.. % getUserObject getAbsolutePath) (rest node-path))]
        (expand-paths tree paths)
        (when-let [row (row-for-path tree path)]
          (.setSelectionRow tree row))))))

(defn load-tree-selection [tree]
  (let [path (read-value-from-prefs clooj-prefs "tree-selection")]
    (set-tree-selection tree path)))

;;;;;;;;;;;;;;;;;;;

(defn get-code-files [dir suffix]
  (let [dir (File. dir)]
    (sort (filter #(.endsWith (.getName %) suffix)
                  (file-seq dir)))))

(defn get-temp-file [^File orig]
  (when orig
    (File. (str (.getAbsolutePath orig) "~"))))

(defn get-projects
  "Load projects from preferences, and return
   a sorted vector."
  []
  (->> (read-value-from-prefs clooj-prefs "project-set")
      set
      (sort-by #(.toLowerCase (.getName (file %))))
      vec))

(defn visible-children
  "Get a vector of a directory's children, if there are any.
   Omits hidden and temporary files."
  [file]
  (->> (.listFiles file)
       (remove #(.startsWith (.getName %) "."))
       (remove #(.endsWith (.getName %) "~"))
       vec))

(defn file-name-text
  "Show a file's name, with *stars* if it is the temp file."
  [file]
  (if (.exists (get-temp-file file))
    (str "*" (.getName file) "*")
    (str (.getName file) "    ")))

(defn file-node
  "Tree node representing a file (possibly a directory)."
  [^File file]
  (let [children (delay (visible-children file))]
    (proxy [DefaultMutableTreeNode] [file]
      (getChildAt [i] (file-node (@children i)))
      (getChildCount [] (count @children))
      (toString [] (file-name-text file))
      (isLeaf [] (not (.isDirectory file))))))

(defn root-node
  "The root tree node, given a vector of project locations."
  [projects]
  (proxy [DefaultMutableTreeNode] []
    (getChildAt [i] (file-node (File. (nth projects i))))
    (getChildCount [] (count projects))
    (toString [] "root")))

(defn file-tree-model [projects]
    (DefaultTreeModel. (root-node projects) false))
    
(defn update-project-tree [tree]
  (let [model (file-tree-model (vec @project-set))]
    (awt-event
      ;(time (do
      (.setModel tree model)
      (save-project-set)
      (load-expanded-paths tree)
      (load-tree-selection tree)
      (save-expanded-paths tree))))
    ;))

(defn get-selected-file-path [app]
  (when-let [tree-path (-> app :docs-tree .getSelectionPaths first)]
    (-> tree-path .getLastPathComponent .getUserObject .getAbsolutePath)))

(defn get-selected-namespace [tree]
  (->> tree .getSelectionPaths first
       .getLastPathComponent .getUserObject .toString))

(defn get-selected-projects [app]
  (let [tree (app :docs-tree)
        selections (.getSelectionPaths tree)]
    (for [selection selections]
      (-> selection .getPath second .getUserObject))))
 
(defn add-project [app project-path]
  (swap! project-set conj project-path))

(defn rename-project [app]
  (when-let [dir (choose-file (app :frame) "Move/rename project directory" "" false)]
    (let [old-project (first (get-selected-projects app))]
      (if (.renameTo (File. old-project) dir)
        (do
          (swap! project-set
                 #(-> % (disj old-project) (conj (.getAbsolutePath dir))))
          (update-project-tree (:docs-tree app)))
        (JOptionPane/showMessageDialog nil "Unable to move project.")))))

(defn remove-selected-project [app]
  (apply swap! project-set disj (map #(.getAbsolutePath %)
                                     (get-selected-projects app)))
  (update-project-tree (app :docs-tree)))     
      
