;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.selection
  (:use [seesaw.util :only [check-args]])
  (:require [seesaw.to-widget]))

;TODO put this somewhere
; I think this is generally useful, but the main reason for its existence is
; to reuse the JList selection implementation in swingx' JXList. It adds
; sorting and filter which obviously changes the mapping between view/model
; indexes.
(defprotocol ViewModelIndexConversion
  (index-to-model [this index])
  (index-to-view [this index]))

(defprotocol Selection
  (get-selection [target])
  (set-selection [target args]))

(extend-protocol Selection
  javax.swing.Action
    (get-selection [target]
      (when-let [s (.getValue target javax.swing.Action/SELECTED_KEY)] [true]))
    (set-selection [target [v]] (.putValue target javax.swing.Action/SELECTED_KEY (boolean v)))

  javax.swing.AbstractButton
    (get-selection [target]      [(.isSelected target)])
    (set-selection [target [v]]  (doto target (.setSelected (boolean v))))

  javax.swing.ButtonGroup
    (get-selection [^javax.swing.ButtonGroup target]
      (when-let [sel (some #(when (.isSelected ^javax.swing.AbstractButton %) %) (enumeration-seq (.getElements target)))]
        [sel]))
    (set-selection [^javax.swing.ButtonGroup target [^javax.swing.AbstractButton v]]
      (if v
        (.setSelected target (.getModel v) true)
        (.clearSelection target))
      target)

  javax.swing.JSlider
    (get-selection [target]     (vector (.getValue target)))
    (set-selection [target [v]] (doto target (.setValue v)))

  javax.swing.JSpinner
    (get-selection [target]     (vector (.getValue target)))
    (set-selection [target [v]] (doto target (.setValue v)))

  javax.swing.JComboBox
    (get-selection [target]     (seq (.getSelectedObjects target)))
    (set-selection [target [v]] (doto target (.setSelectedItem v))))

(defn- list-model-to-seq
  [^javax.swing.ListModel model]
  (map #(.getElementAt model %) (range 0 (.getSize model))))

(defn- list-model-indices
  [model values]
  (let [value-set    (if (set? values) values (apply hash-set values))]
    (->> (list-model-to-seq model)
      (map-indexed #(vector %1 (value-set %2)))
      (filter second)
      (map first))))

(defn- jlist-set-selection
  ([^javax.swing.JList target values]
    (if (seq values)
      (let [indices (map #(index-to-view target %) (list-model-indices (.getModel target) values))]
        (.setSelectedIndices target (int-array indices)))
      (.clearSelection target))
   target))

(extend-protocol ViewModelIndexConversion
  javax.swing.JList
    (index-to-model [this index] index)
    (index-to-view [this index] index))

(extend-protocol Selection
  javax.swing.JList
    (get-selection [target]      (seq (.getSelectedValues target)))
    (set-selection [target args] (jlist-set-selection target args)))


(defn- table-selected-cells
  [^javax.swing.JTable target]
  (let [rows (.getSelectedRows target)
        cols (.getSelectedColumns target)
        nr   (alength rows)
        nc   (alength cols)]
    (cond
     ;; the most flexible case: allows almost arbitrary combinations of rows and
     ;; cols.
     (.getCellSelectionEnabled target)
     (if (= 1 nr nc)
       [(.getValueAt target (aget rows 0) (aget cols 0))]
       (vec (for [r rows] ;; this else is the default
              (vec (for [c cols]
                       (.getValueAt target r c))))))
     ;; here it looks like column only mode.  In this case the user probably
     ;; want the values transposed.  We ignore the selected rows and assume that
     ;; the user wants full columns.
     (.getColumnSelectionAllowed target)
     (vec (for [c cols]
            (vec (for [r (range (.getRowCount target))] 
                   (.getValueAt target r c)))))
     ;; row only mode: just as with columns only always deliver full rows, but
     ;; don't transpose.
     (.getRowSelectionAllowed target)
     (vec (for [r rows]
            (vec (for [c (range (.getColumnCount target))] 
                   (.getValueAt target r c))))))))

;; fixme
;; -- what should happen with (selection! tab [0 1]) in col- or
;; rowmode, i.e. vec arg without multi?
(defn- table-set-selection
  [^javax.swing.JTable target args]
  (cond
   ;; nil clears the selection (selection! tab nil)
   (nil? args) (.clearSelection target)
    ;; true selects all (selection! tab true)
   (true? args) (.selectAll target)
   ;; all other cases should be a seq.
   (seq args)
   (let [rowmode (.getRowSelectionAllowed target)
         colmode (.getColumnSelectionAllowed target)]
     (cond
      (every? integer? args)
      ;; a seq of integers: select rows or columns depending on
      ;; selection model
      ;; (selection! tab {:multi? true} [i_0 ... i_n])
      (do (.clearSelection target)
          (doseq [i args] 
            (if (not rowmode)
              (.addColumnSelectionInterval target i i)
              (.addRowSelectionInterval target i i))))
      ;; a vector with two vectors: the rows and columns to select
      ;; when cell selection is enabled 
      ;; (selection! tab {:multi? true} [[r_0 ... r_n] [c_0 ... c_n]])
      (and rowmode colmode
           (= 2 (count args))
           (every? vector? args))
      (let [[rows cols] args]
          (doseq [r rows] 
            (.addRowSelectionInterval target r r))
          (doseq [c cols] 
            (.addColumnSelectionInterval target c c)))))))
   
(extend-protocol Selection
  javax.swing.JTable
  (get-selection [target] (table-selected-cells target))
  (set-selection [target args] (table-set-selection target args)))

(extend-protocol Selection
  javax.swing.JTree
    (get-selection [target] (seq (map #(seq (.getPath ^javax.swing.tree.TreePath %)) (.getSelectionPaths target))))
    (set-selection [target args]
      (if (seq args)
        target
        (.clearSelection target))))

(extend-protocol Selection
  javax.swing.JTabbedPane
    (get-selection [this]
      (let [i (.getSelectedIndex this)]
        (if (neg? i)
          nil
          [{:content (.getComponentAt this i)
           :title   (.getTitleAt this i)
           :index i}])))
    (set-selection [this [v]]
      (cond
        (nil? v) nil
        (string? v)
          (set-selection this [(.indexOfTab this ^String v)])
        (and (number? v) (>= v 0))
          (.setSelectedIndex this (int v))
        (map? v)
          (set-selection this [(or (:index v) (:title v) (:content v))])
        (instance? java.awt.Component v)
          (.setSelectedComponent this ^java.awt.Component v)
        :else
          (set-selection this [(seesaw.to-widget/to-widget* v)]))))

(extend-protocol Selection
  javax.swing.text.JTextComponent
  (get-selection [target]
    (let [start (.getSelectionStart target)
          end   (.getSelectionEnd target)]
      (if-not (= start end) [[start end]])))
  (set-selection [target [args]]
    (if (integer? args)
      (.select target args args)
    (if-let [[start end] args]
      (.select target start end)
      (.select target 0 0)))))

(defn selection
  ([target] (selection target {}))
  ([target opts]
    (let [s (get-selection target)]
      (if (:multi? opts)
        s
        (first s)))))

(defn selection!
  ([target values] (selection! target {} values))
  ([target opts values]
    (check-args (not (nil? target)) "target of selection! cannot be nil")
    (set-selection
      target
      (if (or (nil? values) (true? values) (:multi? opts)) 
        values 
        [values]))
    target))



