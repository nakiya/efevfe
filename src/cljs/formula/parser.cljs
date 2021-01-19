(ns formula.parser
  (:require [instaparse.core :as insta]))

;;Incomplete. But this suffices as a PoC
(def formula-grammar
  "Taken from here and modified: https://notebooks.gesis.org/binder/jupyter/user/bhugueney-binder-test-y69wi9ts/notebooks/Clojure.ipynb : lang0-parser
   number regex from here: https://stackoverflow.com/questions/2811031/decimal-or-numeric-values-in-regular-expression-validation/39399503#39399503"
  (insta/parser
   "prog = (spaces expr spaces)*;
    <expr> = add-sub;
    <add-sub> = mult-div | add | sub;
    add = add-sub spaces <'+'> spaces mult-div;
    sub = add-sub spaces <'-'> spaces mult-div;
    <mult-div> = factor | mult | div;
    mult = mult-div spaces <'*'> spaces factor;
    div = mult-div spaces <'/'> spaces factor;
    <factor> = number | cellref-1 | <'('> spaces expr spaces <')'>;
    <spaces> = <#'\\s*'>;
    number = #'^[-~]?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)';
    cellref-1 = colid <'.'> rowid;
    cellref-2 = colid <'.'> rowid;
    <rowid> = #'[0-9]+' | ident;
    <colid> = ident;
    <ident> = #'[a-zA-Z]\\w*';
    <funname> = ident;
    arglist = (spaces expr spaces <','> spaces)* expr | cellrange | Epsilon;
    cellrange = cellref-2 <':'> cellref-2;"))

(comment
  (insta/visualize (formula-grammar "SQRT(SUM(78 + A.12) + B.22)"))
  (insta/visualize (formula-grammar "SUM(1,2, 3 , SQRT(SUM(9,8))) + SUM()"))
  (insta/visualize (formula-grammar "2 + 3 * (24 -5)"))
  (insta/visualize (formula-grammar "2 + 3 / 5"))
  (insta/visualize (formula-grammar "5 * 2 - 6"))
  (insta/visualize (formula-grammar "SUM(A.1, A.2, B.1, B.2) * 12"))
  (insta/visualize (formula-grammar "1.34 + 3.45"))
  (insta/visualize (formula-grammar "-0.3 * .56 + -.7"))
  (insta/visualize (formula-grammar "SUM(A.1:B.5)"))
  (insta/visualize (formula-grammar "SUM(t2.Notional:t4.Notional)"))
  (insta/visualize (formula-grammar "45 + ~34"))
  (insta/visualize (formula-grammar "22/11 + SUM(A.1, B.3) * SQRT(A.2:B.3)"))
  (insta/visualize (formula-grammar "22/11 + SUM(A.1, B.3) * SQRT()")))

;; Model is a map here.
(def model {"A.1" 1 "A.2" 2 "A.3" 3 "A.4" 4
            "B.1" 5 "B.2" 6 "B.3" 7 "B.4" 8
            "t1.Notional" 1000 "t2.Notional" 2000 "t3.Notional" 3000})

(def custom-functions
  {"SUM" +
   "SQRT" #(Math/sqrt %)
   "CONCAT" str})

(defn lookup-cell
  ([col row]
   (println "lookup-cell" col row)
   (model (str col "." row)))
  ([[_ col row]]
   (lookup-cell col row)))

(defn call-function
  "Form of the second argument: [:arglist 80 54]. So we destructure here and drop the first item"
  [funname [_ & args]]
  ;(println "call-function:" funname "with args = " args)
  (apply (custom-functions funname) (flatten args)))

(defn expand-range [[_ start-col start-row] [_ end-col end-row]]
  ;(println "unroll-range" start-col "." start-row " to " end-col "." end-row)
  ;There's one concern here: How to unroll something like t1.xxx:t5.xxx. Perhaps handle t[row] specially?
  ;Only do ranges of the form A.xx:C.yy for now.
  (if (= 2 (count (filter #(re-matches #"[A-Z]" %) [start-col end-col])))
    ;;Note that if this is true, row will be a number.
    (let [start-col-i (int (first start-col))
          end-col-i (int (first end-col))
          begin-col-i (if (< start-col-i end-col-i) start-col-i end-col-i)
          term-col-i (if (< start-col-i end-col-i) end-col-i start-col-i)
          col-range (map (comp str char) (range begin-col-i (inc term-col-i)))
          start-row-i (js/parseInt start-row)
          end-row-i (js/parseInt end-row)
          begin-row-i (if (< start-row-i end-row-i) start-row-i end-row-i)
          term-row-i (if (< start-row-i end-row-i) end-row-i start-row-i)
          row-range (map str (range begin-row-i (inc term-row-i)))]
      (->> (for [col col-range row row-range]
             [col row])
           (map #(apply lookup-cell %)))) nil))

(comment
  (expand-range [:cellref "A" "1"] [:cellref "B" "3"])
  (expand-range [:cellref "B" "2"] [:cellref "A" "1"])
  (expand-range [:cellref "B" "1"] [:cellref "A" "3"]))

(def transform-map {:number #(js/parseInt %)
                    :add + :sub - :mult * :div /
                    :colid identity :rowid identity
                    :cellref-1 lookup-cell
                    :cellrange expand-range
                    :funcall call-function
                    :prog identity})

;;We can use zippers or tree walkers here. But instaparse's built in fn suffices here.
(comment
  (insta/transform transform-map (formula-grammar "SUM(A.1,B.1)"))
  (insta/transform transform-map (formula-grammar "SUM(A.1, A.2, B.1, B.2) * 12"))
  (insta/transform transform-map (formula-grammar "22/11 + SUM(A.1, B.3) * SQRT(SUM(A.2:B.3))"))
  (insta/transform transform-map (formula-grammar "SUM(t1.Notional, t3.Notional)")))

;(println (formula-grammar "22/11 + SUM(A.1, B.3) * SQRT(A.2:B.3)"))