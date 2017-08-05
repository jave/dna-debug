;;; dna-debug.el --- functions to assist when debugging your DNA

;; License: GPL-3+

;;; Commentary:

;; DNA is a form of code delivered with your body. it is not Free
;; Software, because even if you get a working system(you) and many
;; copies of the code, present in all your cells, you dont get the
;; tools to actualy read and understand the code.

;; In order to debug your DNA, you must first aquire a machine
;; readable form of your DNA. You can do this from services such as
;; decodeme, or 23andme.

;; The format used by decodeme looks like this:

;; Name,Variation,Chromosome,Position,Strand,YourCode
;; rs1799971,A/G,6,154402490,+,AG
;; ...

;; rs1799971 is an SNP identifier. Not all snp identifiers currently
;; have any meaningful information associated, but this particular
;; identifier you can examine by doing M-X dnabug-snpedia RET with
;; point at the id. A browser window will open(use w3m to browse in Emacs)
;; and you can then find this entry:

;;  rs1799971(A;G) stronger cravings for alcohol; if alcoholic,
;;  naltrexone treatment 2x more successful

;; This means that if you have this particular snp combination you
;; might consider being careful with alcohol consumption.

;; This is currently the only method of DNA debugging, modifying
;; behaviour such as not to trigger undesired parts of the DNA code, or
;; trigger desired parts. Its not currently feasible to rewrite parts
;; of your DNA (except through spawning, which is outside the scope of
;; this Emacs mode)

;;; Code:

(require 'json)

(defun dnabug-snpedia ()
  "Look up the SNP identifier at point at Snpedia. "
  (interactive)
  (browse-url (concat "http://www.snpedia.com/index.php/" (current-word))))


(defvar dnabug-snpedia-snps nil)
(defvar dnabug-snps)

(defun dnabug-read-snpedia-snps ()
  "Build an index of all known SNP:s at Snpedia.
Snpedia uses the mediawiki api, and theres a limit of data per request.
The start, callback, and finaly methods are called in turn to build the index. "
  (setq dnabug-snpedia-snps nil)
  (setq dnabug-snps nil)  
  (dnabug-read-snpedia-snps-with-start "")
    )


(defun  dnabug-read-snpedia-snps-with-start (start)
  "Start building and index from snpedia. "
  (url-retrieve (concat
                 "http://www.snpedia.com/api.php?action=query&list=categorymembers&cmtitle=category:is_a_snp&cmlimit=5000&format=json&cmcontinue="
                 start)
                'dnabug-read-snpedia-snps-callback))


(defun dnabug-read-snpedia-snps-callback (status)
  "Handle each batch of snp:s indexed from snpedia. "
  (search-forward "\n\n")
  (let* ((x (json-read))
         (snps-next))
    (setq snps-next (cdr (assoc 'cmcontinue (assoc 'categorymembers (assoc 'query-continue x)))))
    (setq dnabug-snpedia-snps (vconcat dnabug-snpedia-snps (cdr (assoc 'categorymembers (assoc 'query x)))))
    (if snps-next
        (dnabug-read-snpedia-snps-with-start  snps-next)
       (dnabug-read-snpedia-snps-finaly))
  ))

(defun dnabug-read-snpedia-snps-finaly ()
  "finalize building of index from snpedia. "
  (mapc
   (lambda (el) (setq dnabug-snps (plist-put dnabug-snps (downcase (cdar el)) t)))
   dnabug-snpedia-snps)
  )


(defun dnabug-font-lock (limit)
  (while 
      (and (search-forward-regexp "\\<\\(rs[0-9]+\\)\\>" limit)
           (not (lax-plist-get dnabug-snps (match-string 0)))))
    (lax-plist-get dnabug-snps (match-string 0))
  )

;;( dnabug-read-snpedia-snps)
;;(font-lock-add-keywords nil '((dnabug-font-lock . font-lock-keyword-face)) )

;;TODO provide minor mode which applies the font lock function to a buffer

(provide 'dna-debug)
