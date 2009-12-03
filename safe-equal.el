;;; safe-equal.el --- Test even circular objects for equality

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <Tehom@localhost>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This also runs in Common Lisp, but various tree-walking is not
;; complete, since I just moved it over.  For one thing, it doesn't
;; understand Common Lisp structures.

;;; Code:

;;;;;;;;;;;;;;;;;
;;;; Requirements

(when (boundp 'emacs-version)
  (require 'cl))


(defun safe-equal-read-equivalent-p
  (object)
  "non-nil if OBJECT is read-equivalent, ie remains `eql' when printed
out and read back in."
  (or
    ;; Numbers are read-equivalent
    (numberp object)
    ;;Strings are read-equivalent
    (stringp object) 
    ;; Interned symbols are read-equivalent
    (and
      (symbolp object)
      (eq object
	(intern-soft
	  (symbol-name object))))))

(defun safe-equal-obj-seen-position (ob all-objs-seen)
  "Return the position of OB in the seen list, if any.
As a side-effect, this makes OB seen in all-objs-seen"
   (cond
     ;;read-equivalent objects can never be circular.
     ((safe-equal-read-equivalent-p ob)
       nil)
     
     ;;Objects that are already seen return their position
     ((position ob (cdr all-objs-seen) :test #'eq))

     ;;Unseen objects are henceforth considered seen.
     (t
       (push ob (cdr all-objs-seen))
       nil)))

(eval-when-compile
  (defvar se-literal 234 "" )
  (defvar se-foo '(1 2)  "" )

  (setf
    (get 'safe-equal-obj-seen-position 'rtest-suite)
    '("safe-equal-obj-seen-position"
       ( "Literal objects are never considered already seen"
	 (safe-equal-obj-seen-position se-literal 
	   (list nil se-literal))
	 nil)
       
       ( "Non-literal objects are already seen if they're on the list"
	 (safe-equal-obj-seen-position se-foo (list nil se-foo))
	 0)

       ( "Non-literal objects not are already seen if they're not on
the list" 
	 (safe-equal-obj-seen-position se-foo (list nil))
	 nil)

       
       )))



(defun safe-equal-init-seen-objs ()
  ""
  (list nil))


;;;;;;;;;;;;;
;;Entry point

;;Special variables
(defvar circle-table-0 nil "" )
(defvar circle-table-1 nil "" )

;;;###autoload
(defun safe-equal (ob0 ob1)
  ""

  (let*
    (
      (circle-table-0 (safe-equal-init-seen-objs))
      (circle-table-1 (safe-equal-init-seen-objs)))
    (declare (special circle-table-0 circle-table-1))
    (safe-equal-compare-objects-circular ob0 ob1)))


;;Only this, the root worker, needs to consider circularity.  We
;;definitely need to consider where it is in the list, otherwise we
;;rue the risk of declaring objects equal that have different
;;circularities, eg (#0=A #1=B #0# #1#) vs (#0=A #1=B #1# #0#)
(defun safe-equal-compare-objects-circular (ob0 ob1)
  ""
  (declare (special circle-table-0 circle-table-1))
  (let 
    (
      (tag-0 (safe-equal-obj-seen-position ob0 circle-table-0))
      (tag-1 (safe-equal-obj-seen-position ob1 circle-table-1)))

    (cond
      ;;If the object is repeated in both lists...
      ( (and tag-0 tag-1)

	;;The positions mismatch iff the structure so far does.  This
	;;may seem insufficient, but if this is the first back edge
	;;then equal position numbers refer to the same respective
	;;position in both structures, and by this test we maintain
	;;that invariant, so by induction equal positions always imply
	;;the same structure.

	(= tag-0 tag-1))
      
      
      ;;Either one standing alone indicates a mismatch.
      (tag-0 nil)
      (tag-1 nil)

      ;;If neither is repeated, simply compare them
      (t (safe-equal-compare-objects ob0 ob1)))))



;;Adapted from cust-print's cust-print-print-object by LaLiberte
(defun safe-equal-compare-objects (ob0 ob1)
  (cond 
    ((null ob0)    
      (null ob1))

    ((consp ob0)   
      (and
	(consp ob1)
	(safe-equal-compare-cons ob0 ob1)))
    
    ((vectorp ob0) 
      (and
	(vectorp ob1)
	(safe-equal-compare-vectors ob0 ob1)))

    ((stringp ob0)
      (and
	(stringp ob1)
	(string= ob0 ob1)))
    
    ;; For atomic types, just compare with eql, which can't stall.
    (t 
      (eql ob0 ob1))))

(eval-when-compile
  (setf
    (get 'safe-equal-compare-objects 'rtest-suite)
    '("safe-equal-compare-objects"
       ( "The same number compares equal"
	 (safe-equal-compare-objects 12 12)
	 t)

       ( "The same string compares equal"
	 (safe-equal-compare-objects "abc" "abc")
	 t)

       ;;safe-equal-compare-objects should inherit
       ;;safe-equal-compare-cons' and safe-equal-compare-vectors'
       ;;tests.
       
       )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; List and vector processing for compare functions.


;;Efficiency, especially not using a lot of stack, would come from
;;making the end recursion into a while loop.  But IIUC the compiler
;;already does that.
(defun safe-equal-compare-cons (cons-0 cons-1)

  (and
    (safe-equal-compare-objects-circular (car cons-0) (car cons-1))
    (safe-equal-compare-objects-circular (cdr cons-0) (cdr cons-1))))



(defun safe-equal-compare-vectors (vec-0 vec-1)
  ""
  (let 
    (
      (len-0 (length vec-0))
      (len-1 (length vec-1)))
    
    (if
      (/= len-0 len-1)
      ;;If their lengths are different, mismatch.
      nil

      ;;Compare each element.
      (dotimes (i len-0 t)
	;;Stop immediately on a mismatch.
	(if
	  (not
	    (safe-equal-compare-objects-circular 
	      (aref vec-0 i) 
	      (aref vec-1 i)))
	  (return nil))))))


;;;;;;;;;
;;Testing

(eval-when-compile

  ;;Many of the tests use this.
  (defconst se-let-circe-tables
    '(:around
       (let
	 (
	   (circle-table-0 (safe-equal-init-seen-objs))
	   (circle-table-1 (safe-equal-init-seen-objs)))))
    
    "" )

  ;;Define some circular objects
  (defvar safe-equal-test-circular-obj-0 (list 0 0) "" )
  (setf (car safe-equal-test-circular-obj-0) safe-equal-test-circular-obj-0)

  (defvar safe-equal-test-circular-obj-1 (list 0 1) "" )
  (setf (car safe-equal-test-circular-obj-1) safe-equal-test-circular-obj-1)

  (defvar safe-equal-test-circular-obj-2 (list 0 0) "" )
  (setf (car safe-equal-test-circular-obj-2) safe-equal-test-circular-obj-2)


  ;;DANGER: This dies, so do not eval it *or* unquote it unless you
  ;;need to demonstrate that `equal' dies when trying to compare
  ;;circular objects.
  '(equal safe-equal-test-circular-obj-0 safe-equal-test-circular-obj-1)


  (setf
    (get 'safe-equal-rtest 'rtest-suite)
    '("safe-equal-rtest"

       safe-equal-obj-seen-position
       safe-equal-compare-objects

       ;;Obsolete
       ;;cust-print-preprocess-circle-tree
       '("Ordinary objects have empty circularity tables"
	  (cust-print-preprocess-circle-tree '(0 0))
	  nil)
       
       ;;Obsolete
       ;;safe-equal-circle-tables-same-structure-p
       '("For objects with similar structure, their circularity tables
compare t" 
	  (safe-equal-circle-tables-same-structure-p
	    (cust-print-preprocess-circle-tree 
	      safe-equal-test-circular-obj-0) 
	    (cust-print-preprocess-circle-tree
	      safe-equal-test-circular-obj-1))
	  t)
       
       '( ""
	  (safe-equal-circle-tables-same-structure-p
	    (cust-print-preprocess-circle-tree 
	      0) 
	    (cust-print-preprocess-circle-tree
	      safe-equal-test-circular-obj-1))
	  nil)


       ;;safe-equal-compare-cons
       ( "Equivalent lists with no circularity give t"
  
	 (safe-equal-compare-cons '(0 0) '(0 0))
	 t
	 :include se-let-circe-tables)

       ( "Unequivalent lists with no circularity give nil"
	 (safe-equal-compare-cons '(0 0) '(0 1))

	 nil
	 :include se-let-circe-tables)

       ( "Equivalent dotted lists give t"
	 (safe-equal-compare-cons '(0 . 0) '(0 . 0))

	 t
	 :include se-let-circe-tables)
       

       ( "Unequivalent dotted lists give nil"
	 (safe-equal-compare-cons '(0 . 0) '(0 . 1))

	 nil
	 :include se-let-circe-tables)
       
       ( "Equivalent circular lists give t"
	 (safe-equal-compare-cons 
	   safe-equal-test-circular-obj-0
	   safe-equal-test-circular-obj-2)

	 t
	 :include se-let-circe-tables)

       ( "Unequivalent circular lists give nil"
	 (safe-equal-compare-cons 
	   safe-equal-test-circular-obj-0
	   safe-equal-test-circular-obj-1)

	 nil
	 :include se-let-circe-tables)

       ;;safe-equal-compare-vectors
       
       ( "Equivalent vectors with no circularity give t"
	 (safe-equal-compare-vectors [0 0] [0 0])

	 t
	 :include se-let-circe-tables)

       ( "Unequivalent vectors with no circularity give nil"
	 (safe-equal-compare-vectors [0 0] [0 1])

	 nil
	 :include se-let-circe-tables)



       ;;safe-equal-compare-objects-circular
       ( "Equivalent circular lists give t"
	 (safe-equal-compare-objects-circular
	   safe-equal-test-circular-obj-0
	   safe-equal-test-circular-obj-2)
	 t
	 :include se-let-circe-tables)

       ( "Unequivalent circular lists give nil"
	 (safe-equal-compare-objects-circular
	   safe-equal-test-circular-obj-0
	   safe-equal-test-circular-obj-1)

	 nil
	 :include se-let-circe-tables)

       
       ;;safe-equal
       ( "Equivalent simple objects give t"
	 (safe-equal 0 0)
	 t)

       ( "Unequivalent simple objects give nil"
	 (safe-equal 0 1)
	 nil)
       
       ( "Equivalent complex objects give t"
	 (safe-equal '(0 0) '(0 0))
	 t)

       ( "Unequivalent complex objects give nil"
	 (safe-equal '(0 0) '(0 1))
	 nil)

       ( "Equivalent objects with circular structure give t"
	 (safe-equal 
	   safe-equal-test-circular-obj-0 safe-equal-test-circular-obj-2)
	 t)
       
       
       ( "Unequivalent objects with the same circular structure give nil"
	 (safe-equal 
	   safe-equal-test-circular-obj-0 safe-equal-test-circular-obj-1)
	 nil)
       

       )))




(provide 'safe-equal)

;;; safe-equal.el ends here