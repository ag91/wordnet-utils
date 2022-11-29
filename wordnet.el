(require 'dash)
(require 's)

(defun wordnet-extract-hypernyms (command-output)
  (ignore-errors
    (--> command-output
         (s-split "=>" it)
         cdr
         (--map
          (--> (s-split "\n" it t)
               car
               (s-split "," it t))
          it)
         -flatten
         (-map 's-trim it))))

(defun wordnet-extract-synonyms (command-output)
  (ignore-errors
    (--> command-output
         (s-split "Sense" it t)
         (--keep (and (s-contains-p "=>" it)
                      (s-split "," (nth 1 (s-lines it)) t))
                 it)
         -flatten
         -distinct
         (-map 's-trim it))))

(defun wordnet-synonym-command (word &optional n-or-v-or-a-or-r)
  (--> word
       (format "wn %s -syns%s" it (or n-or-v-or-a-or-r "n"))
       (shell-command-to-string it)))

(defun wordnet-derivative-command (word &optional n-or-v)
  (--> word
       (format "wn %s -deri%s" it (or n-or-v "v"))
       (shell-command-to-string it)))

(defun wordnet-verb-derivative (word) (wordnet-extract-hypernyms (wordnet-derivative-command word)))

(defun wordnet-noun-derivative (word) (wordnet-extract-hypernyms (wordnet-derivative-command word "n")))

(defun wordnet-noun-hypernyms (word)
  (wordnet-extract-hypernyms (wordnet-synonym-command word "n")))

(defun wordnet-verb-hypernyms (word)
  (wordnet-extract-hypernyms (wordnet-synonym-command word "v")))

(defun wordnet-attribute-hypernyms (word)
  (wordnet-extract-hypernyms (wordnet-synonym-command word "a")))

(defun wordnet-noun-synonyms (word)
  (-remove-item word (wordnet-extract-synonyms (wordnet-synonym-command word "n"))))

(defun wordnet-verb-synonyms (word)
  (-remove-item word (wordnet-extract-synonyms (wordnet-synonym-command word "v"))))

(defun wordnet-attribute-synonyms (word)
  (-remove-item word (wordnet-extract-synonyms (wordnet-synonym-command word "a"))))

(defun wordnet-?-synonyms (word)
  (-remove-item word (wordnet-extract-synonyms (wordnet-synonym-command word "r"))))


;; (wordnet-noun-synonyms "flower")
;; (wordnet-verb-synonyms "flower")
;; (wordnet-attribute-synonyms "flower")
;; (wordnet-?-synonyms "flower")


(defun wordnet-find-synonyms (word)
  (--> word
       s-lower-camel-case
       s-trim
       (append
        (list it)
        (wordnet-verb-derivative it)
        (wordnet-noun-synonyms it)
        (wordnet-verb-synonyms it)
        (wordnet-attribute-synonyms it)
        (wordnet-?-synonyms it))
       -distinct))

;; (wordnet-find-synonyms "dog") ;; note: adding verb derivative makes "to dog" into "hunt"...

(defun wordnet-find-synonyms-last-word ()
  (wordnet-find-synonyms
   (buffer-substring-no-properties
    (save-excursion (backward-word) (forward-word) (point))
    (save-excursion (backward-word) (point)))))

(defun wordnet-insert-synoyms-last-word-for-ag ()
  "Insert synonyms of last word for AG search."
  (interactive)
  (insert (concat "|" (s-replace-all '((" " . "\\ ") ; necessary to avoid spaces to be treated as an OR in same file (e.g., "water | my house" would make ag union results for "water" and results that match both "my" and cut this result set with the ones also containing "house" resulting in very few matches)
                                       ("(" . "\\(") (")" . "\\)") ("[" . "\\[") ("]" . "\\]"))
                                     (s-join "|" (wordnet-find-synonyms-last-word))))))
(global-set-key (kbd "C-c s i")  'wordnet-insert-synoyms-last-word-for-ag)

(defun wordnet-find-synonyms-last-line ()
  (--> (buffer-substring-no-properties
        (save-excursion (backward-sentence) (forward-sentence) (point))
        (save-excursion (backward-sentence) (point)))
       (s-replace "[!-/:-@[-`{-~]" "" it) ; remove punctuation
       (s-split " " it)
       (-map
        'wordnet-find-synonyms
        it)
       -flatten))
