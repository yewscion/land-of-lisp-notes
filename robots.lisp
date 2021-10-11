(defun robots ()
  (loop named main ; By naming our loop "main", we can use "return
                                        ; from" to exit the loop early.
        with directions = '((q . -65) (w . -64) (e . -63)
                            (a . -1)            (d . 1)
                            (z . 63)  (x . 64)  (e . 65)) ; These are
                                        ; apparently the offsets for a
                                        ; 64-wide board; I've arranged
                                        ; them according to the keys
                                        ; they are assigned to.
        for pos = 544
        then (progn (format t "~%(qwe/asd/zxc) to move, (t)eleport, (l)eave:")
                    (force-output)
                    (let* ((c (read))
                           (d (assoc c directions))); Perform a lookup
                                        ; of the input
                                        ; with assoc.
                      (cond (d (+ pos (cdr d)))
                            ((eq 't c) (random 1024))
                            ((eq 'l c) (return-from main 'bye)); Player
                                        ; wants to leave.
                            (t pos))))
        for monsters = (loop repeat 10
                             collect (random 1024)) ;; 10 robots,
        ;; random places.
        then (loop for mpos in monsters
                   collect (if (> (count mpos monsters) 1)
                               mpos
                               (cdar (sort (loop for (k . d) in directions
                                                 for new-mpos = (+ mpos d)
                                                 ; This is to
                                                 ; calculate something
                                                 ; called the
                                                 ; "Manhattan
                                                 ; Distance" to the
                                                 ; player. I will need
                                                 ; to look this up for
                                                 ; future reference.
                                                 collect (cons (+ (abs (- (mod new-mpos 64)
                                                                          (mod pos 64)))
                                                                  (abs (- (ash new-mpos -6)
                                                                          (ash pos -6))))
                                                               new-mpos))
                                           '<
                                           :key #'car))))
        when (loop for mpos in monsters
                   always (> (count mpos monsters) 1))
        return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"; Was told not to worry
                                        ; about this yet; it looks
                                        ; very similar to the table
                                        ; formula above, will probably
                                        ; dissect when done typing
                                        ; this in.

                   (loop for p
                         below 1024 ; Loop through board positions.
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses))
                                              ((> (count p monsters) 1) #\#)
                                              (t #\A)))
                                        ((= p pos) #\@)
                                        (t #\ ))))))
