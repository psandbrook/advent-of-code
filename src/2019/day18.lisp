(defpackage aoc/2019/day18
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util)
  (:import-from :str :concat :join :split :trim))
(in-package :aoc/2019/day18)

;;; Part 1

(defun parse-map (str)
  (let* ((lines (split #\Newline str :omit-nulls t))
         (map (make-array (list (length lines) (length (first lines))) :initial-element nil)))
    (iter (for l in lines)
          (for y below (array-dimension map 0))
          (iter (for c in-string l)
                (for x below (array-dimension map 1))
                (setf (aref map y x)
                      (case c
                        (#\@ 'entrance)
                        (#\. 'space)
                        (#\# 'wall)
                        (otherwise (assert (alpha-char-p c))
                                   c)))))
    map))

(defun traversable (tile keys)
  (or (member tile '(entrance space))
      (and (typep tile 'character)
           (or (lower-case-p tile)
               (member (char-downcase tile) keys)))))

(defun find-entrance (map)
  (iter outer (for y below (array-dimension map 0))
        (iter (for x below (array-dimension map 1))
              (if (eq (aref map y x) 'entrance) (return-from outer (list y x))))))

(defun reachable-keys (map path)
  (destructuring-bind (path-keys path-distance pos) path
    (let ((out-paths ())
          (discovered (make-array (array-dimensions map) :initial-element nil))
          (q ()))
      (setf (apply #'aref discovered pos) t)
      (push (list pos 0) q)
      (iter (while (not (emptyp q)))
            (for (pos distance) = (rpop q))
            (for tile = (apply #'aref map pos))
            (when (and (typep tile 'character)
                       (lower-case-p tile)
                       (not (member tile path-keys)))
              (push (list (cons tile path-keys) (+ distance path-distance) pos)
                    out-paths))

            (for (pos-y pos-x) = pos)
            (for neighbours = (list (list (1- pos-y) pos-x)
                                    (list (1+ pos-y) pos-x)
                                    (list pos-y (1- pos-x))
                                    (list pos-y (1+ pos-x))))
            (iter (for n in neighbours)
                  (when (and (apply #'array-in-bounds-p map n)
                             (traversable (apply #'aref map n) path-keys)
                             (not (apply #'aref discovered n)))
                    (setf (apply #'aref discovered n) t)
                    (push (list n (1+ distance)) q))))
      out-paths)))

(defun delete-bad-paths (paths)
  (let ((out-paths ())
        (seen (make-hash-table :test #'equal)))
    (iter (for path in paths)
          (for (keys distance pos) = path)
          (setf keys (sort (copy-list keys) #'char<))
          (for seen-distance = (gethash (list keys pos) seen))
          (if (or (not seen-distance) (< distance seen-distance))
              (setf (gethash (list keys pos) seen) distance)))

    (iter (for ((keys pos) distance) in-hashtable seen)
          (push (list keys distance pos) out-paths))
    out-paths))

(defun shortest-path-all-keys (map)
  (let ((paths (reachable-keys map (list () 0 (find-entrance map)))))
    (iter
      (for new-paths = (lparallel:pmapcan (lambda (path) (reachable-keys map path)) paths))
      (if (emptyp new-paths) (finish))
      (setf paths (delete-bad-paths new-paths)))
    (apply #'min (mapcar #'second paths))))

(deftest part-1-test
  (testing "shortest-path-all-keys"
           (ok (= (shortest-path-all-keys (parse-map (concat "########################" *newline-str*
                                                             "#f.D.E.e.C.b.A.@.a.B.c.#" *newline-str*
                                                             "######################.#" *newline-str*
                                                             "#d.....................#" *newline-str*
                                                             "########################")))
                  86))

           (ok (= (shortest-path-all-keys (parse-map (concat "########################" *newline-str*
                                                             "#...............b.C.D.f#" *newline-str*
                                                             "#.######################" *newline-str*
                                                             "#.....@.a.B.c.d.A.e.F.g#" *newline-str*
                                                             "########################")))
                  132))

           (ok (= (shortest-path-all-keys (parse-map (concat "#################" *newline-str*
                                                             "#i.G..c...e..H.p#" *newline-str*
                                                             "########.########" *newline-str*
                                                             "#j.A..b...f..D.o#" *newline-str*
                                                             "########@########" *newline-str*
                                                             "#k.E..a...g..B.n#" *newline-str*
                                                             "########.########" *newline-str*
                                                             "#l.F..d...h..C.m#" *newline-str*
                                                             "#################")))
                  136))

           (ok (= (shortest-path-all-keys (parse-map (concat "########################" *newline-str*
                                                             "#@..............ac.GI.b#" *newline-str*
                                                             "###d#e#f################" *newline-str*
                                                             "###A#B#C################" *newline-str*
                                                             "###g#h#i################" *newline-str*
                                                             "########################")))
                  81))))

;;; Part 2

(defun find-entrances (map)
  (let ((entrances ()))
    (iter (for y below (array-dimension map 0))
          (iter (for x below (array-dimension map 1))
                (if (eq (aref map y x) 'entrance) (push (list y x) entrances))))
    entrances))

(defun update-map (init-map)
  (let ((init-entrance-pos (find-entrance init-map))
        (map (copy-array init-map)))
    (destructuring-bind (y x) init-entrance-pos
      (setf (aref map (1- y) (1- x)) 'entrance
            (aref map (1- y) x) 'wall
            (aref map (1- y) (1+ x)) 'entrance
            (aref map y (1- x)) 'wall
            (aref map y x) 'wall
            (aref map y (1+ x)) 'wall
            (aref map (1+ y) (1- x)) 'entrance
            (aref map (1+ y) x) 'wall
            (aref map (1+ y) (1+ x)) 'entrance))
    map))

(defun shortest-path-all-keys-2 (map)
  (let ((paths (list (list () 0 (find-entrances map)))))
    (iter
      (for new-paths = (lparallel:pmapcan
                         (lambda (path)
                           (destructuring-bind (keys distance poss) path
                             (iter (for pos in poss)
                                   (for i from 0)
                                   (for out-paths = (reachable-keys map (list keys distance pos)))
                                   (iter (for path in out-paths)
                                         (for (nil nil pos) = path)
                                         (setf (third path) (copy-list poss))
                                         (setf (nth i (third path)) pos))
                                   (nconcing out-paths))))
                         paths))
      (if (emptyp new-paths) (finish))
      (setf paths (delete-bad-paths new-paths)))
    (apply #'min (mapcar #'second paths))))

(deftest part-2-test
  (testing "shortest-path-all-keys-2"
           (ok (= (shortest-path-all-keys-2 (parse-map (concat "#############" *newline-str*
                                                               "#DcBa.#.GhKl#" *newline-str*
                                                               "#.###@#@#I###" *newline-str*
                                                               "#e#d#####j#k#" *newline-str*
                                                               "###C#@#@###J#" *newline-str*
                                                               "#fEbA.#.FgHi#" *newline-str*
                                                               "#############")))
                  32))

           (ok (= (shortest-path-all-keys-2 (parse-map (concat "#############" *newline-str*
                                                               "#g#f.D#..h#l#" *newline-str*
                                                               "#F###e#E###.#" *newline-str*
                                                               "#dCba@#@BcIJ#" *newline-str*
                                                               "#############" *newline-str*
                                                               "#nK.L@#@G...#" *newline-str*
                                                               "#M###N#H###.#" *newline-str*
                                                               "#o#m..#i#jk.#" *newline-str*
                                                               "#############")))
                  72))))
