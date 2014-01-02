(require-extension srfi-1)
(require-extension data-structures)
(require-extension sqlite3)
(require-extension irregex)
(require-extension sql-null)
(require-extension message-digest)
(require-extension sha2)
(require-extension srfi-19-time)
(use spiffy)

(define database-path "/srv/http/sr4.ef.gy/db/sr4.sqlite3")

(define sql-type->string
  (lambda (sql-data)
    (cond
     ((string? sql-data) sql-data)
     ((boolean? sql-data) (number->string sql-data))
     ((fixnum? sql-data) (number->string sql-data))
     ((real? sql-data) (number->string sql-data))
     ((blob? sql-data) "BLOB")
     ((sql-null? sql-data) ""))))

(define get-name 
  (lambda (id)
     (first-result (open-database database-path)
                   "select Name from Characters where CharacterID=?"
                   id)))

(define (has-group skill id)
    (= 1 (first-result (open-database database-path)
		  "select exists(select CharacterID from CharacterSkillGroups inner join Skills on CharacterSkillGroups.SkillGroup = Skills.SkillGroup where Skills.Name = ? and CharacterID = ?)" skill id)))


;; I've got to do it this way because I can't rely on the same connection being open when I call this function
(define get-last-rowid
   (lambda (table)
     (first-result (open-database database-path)
                   (string-append "select max(rowid) from "table
                   ))))
                   
(define comma-separated-strings
 (lambda (strings)
    (let
      ((strings-with-commas (apply string-append (map (lambda (s) (string-append s ", ")) strings))))
      (substring strings-with-commas 0 (- (string-length strings-with-commas) 2)))))
     
(define prepare-input
   (lambda (table-name column-names)
    (let* 
      ((column-amount (length column-names))
      (assemble-question-marks (comma-separated-strings (map (lambda (x) "?") column-names)))
      )
     (string-append "insert into "
                    table-name
                    " ("
                    (comma-separated-strings column-names)
                    ") values ("
                    assemble-question-marks 
                    ;; assemble correct amount of ? s
                    ")")))) ;; i need to pass values as parameters

(define display-single-row
  (lambda row
    (letrec
      ((display-cell (lambda (col) (string-append "<td>" (sql-type->string col) "</td>"))))
        (string-append "<tr>"
                       (apply string-append (map display-cell row))
                       "</tr>"
         ))))
         
(define display-single-row-numbers-only
  (lambda row
    (letrec
      ((display-cell (lambda (col) (string-append "<td>" (number->string col) "</td>"))))
        (string-append "<tr>"
                       (apply string-append (map display-cell row))
                       "</tr>"
         ))))
    
(define display-entire-table
  (lambda (table . columns)
     (let* ((cols (apply string-append (map (lambda (s) (string-append s ", ")) columns)))
     (query (string-append "select " 
                                  (substring cols 0 (- (string-length cols) 2))
                                  " from "
                                  table
                                  ";"))
            (db (open-database database-path))
      (table (apply string-append (map-row
         display-single-row
         db
         query))))
       (if (> (string-length table) 0) 
	(string-append "<table>" table
       "</table>")))))
       

(define prefix-string
      "Content-Type: application/xhtml+xml\r\n\r\n<?xml version='1.0' encoding='UTF-8'?>") 

(define html-string
      "<html xmlns='http://www.w3.org/1999/xhtml'>")

(define (html-header title)
   (string-append "<head><title>" title "</title></head>"))

(define (html-page title page-elements)
   (string-append prefix-string
		  html-string
		  (html-header title)
		  "<body>"
		  (fold-right string-append ""  page-elements)
 		  "</body></html>"))


(define (create-html-paragraph text)
  (string-append "<p>" text "</p>"))

;; creates a HTML caption: <hn>title</hn>, with n an integer   
(define (create-html-caption n title)
   (let ((hn (string-append "h" (number->string n))))
   (string-append "<" hn ">" title "</" hn ">")))

       
(define create-html-table
  (lambda (title captions query id)
    (let* ((db (open-database database-path))
	  (table (string-append "" (apply string-append (map-row display-single-row
                                     db
                                     query
                                     id))))
	  (is-string (string? table)))
    (if (and is-string
	     (> (string-length table) 0))
      (string-append 
        (create-html-caption 1 title)
        "<table>"
        "<tr>"
        (apply string-append (map (lambda (c) (string-append "<th>" c "</th>")) captions))
        "</tr>"
        table
         "</table>")
      ""))))

(display (create-html-table "foo" (list "x") "select Rating from CharacterComplexForms where CharacterID=?" 1)) 


(define (create-html-link url class text )
  (string-append "<a href=\"" url "\" class=\"" class "\">" text "</a>"))

;; creates internal link with CGI params
;; class: html attribute
;; sub: http://sr4.ef.gy/sub?args
;; text: link text
;; params: list of pairs (arg, value)
(define (create-internal-link class sub text params)
 (let* ((str (fold-right string-append 
           ""                              
           (map (lambda (v) (string-append (->string (car v)) "=" (->string (cdr v)) "&amp;")) params)))
	(url (string-append "http://sr4.ef.gy/" sub "?" 
			    (substring str 0 (- (string-length str) 5)))))
 (create-html-link  url
    class (if (string=? text "") url text))))



 
(define create-html-form
   (lambda (action . form-elements)
      (string-append
      "<form "
      (if (> (string-length action) 0)
        (string-append "action=\"" action "\"")
        "")
      " method=\"get\">"
      (apply string-append form-elements)
      "</form>")))


;; todo: hash character ID 
(define (create-character-link id hashed admin)
  (if hashed 
	(create-internal-link "" "characters" "" (list (cons "page" "characters") (cons "hash" (->string id)) (cons "admin" (if admin "true" "false"))))
	(create-internal-link "" "characters" "" (list (cons "page" "characters") (cons "cid" (->string id))))))
      

(define-record option value text)
      
(define create-html-select
    (lambda (label name  options)
       (string-append
       "<p><label for=\""
       name
       "\">"
       label
       "</label>"
       "<select name=\""
       name
       "\">"
       (apply string-append
         (map
          (lambda (op)
             (string-append "<option value=\"" (option-value op) "\">" (option-text op) "</option>\n"))
          options))
        "</select></p>")))
        
(define create-html-input
     (lambda (name label default)
        (string-append "<p><label for=\""
        name
        "\">"
        label
        "</label> <input type=\"text\" name=\""
        name
        "\" value=\""
        default
        "\" /></p>")))
        
(define create-html-submit 
    (lambda (name value)
      (string-append "<p><input type=\"submit\" name=\""
      name
      "\" value=\""
      value
      "\" /></p>")))

(define (escape-ampersands str)
   (irregex-replace/all "&" str "&amp;"))
  


; we need the extra description because of the Body attribute 
(define-record attribute name description value)

(define (create-numeric-value-form id title values subject act)
  (string-append (create-html-caption 1 title)
    (create-html-form 
      "edit-attributes"
     (fold-right string-append "" 
       (concatenate
	  (map (lambda (v)
                 (list   (create-html-input (attribute-name v) (attribute-description v) (sql-type->string (attribute-value v)))
                           (create-internal-link "buttons" "edit" "++" (list (cons "page" "edit-character") (cons "action"  "get") (cons subject  (attribute-name v)) (cons act  "inc") (cons "cid"  id)))
                           (create-internal-link "buttons" "edit" "--" (list (cons "page" "edit-character") (cons "action"  "get") (cons subject  (attribute-name v)) (cons act  "dec") (cons "cid"  id)))))
                values))))))

(define-record skill name value group)

(define (create-skills-form id title values groups subject act)
   (string-append (create-html-caption 1 title)
	(create-html-form
	"edit-skills"
        (fold-right string-append ""
	   (concatenate
		(map (lambda (v) 
			(list (create-html-input (skill-name v) (skill-name v) (sql-type->string (skill-value v)))
                           (create-internal-link "buttons" "edit" "++" (list (cons "page" "edit-character") (cons "action"  "get") (cons subject  (skill-name v)) (cons act  "inc") (cons "cid"  id)))
                           (create-internal-link "buttons" "edit" "--" (list (cons "page" "edit-character") (cons "action"  "get") (cons subject  (skill-name v)) (cons act  "dec") (cons "cid"  id)))
			   (create-html-paragraph (skill-group v))
				))
                values))))))

			     

(define (current-resources id)
  (create-html-table "Resources" 
                     (list "Total Karma" "Current Karma" "Total BP" "BP left")
		     "select TotalKarma, CurrentKarma, TotalBP, TotalBP - BP from Characters inner join ViewTotalCost on Characters.CharacterID = ViewTotalCost.CharacterID where Characters.CharacterID=?" id)) 
        
;; I definitely need a way for the user to get an email or something after character creation
(define (access-links hash admin-hash)
  (string-append (create-html-paragraph (string-append "Go to the following URL to view your character: " (create-character-link hash #t #f)))
		 (create-html-paragraph (string-append "Go to the following URL to edit your character: " (create-character-link admin-hash #t #t)))
		 (create-html-paragraph "Please bookmark these URLs, as they are the only way to access your character from now on. In the future, the web site will automatically send you an email after character creation, but this feature is not implemented yet - sorry!")))

(define (get-id-from-hash hash admin)
  (first-result (open-database database-path)
		(string-append "select CharacterID from Characters where " (if admin "AdminHash" "Hash") " = ?") hash))


(define (display-character-html hash admin-hash) 
  (let* (
     (id (sql-type->string (get-id-from-hash (->string hash) #f)))
     (name (get-name id))
     (s-hash (->string hash))
     (s-admin-hash (->string admin-hash)))
     (if (not (string=? id "0"))
	(html-page name
    	 (list 
      	  (create-html-caption 1 name)
    	  (current-resources id)
	     (if (and (> (string-length s-hash) 0) 
		      (> (string-length s-admin-hash) 0)) (access-links s-hash s-admin-hash) "" ) 
   	  (create-html-table "" (list "Description") "select Description from Characters where CharacterID=?" id)
     	  (create-html-table "Attributes" (list "Body" "Agility" "Strength" "Reaction" "Logic" "Intuition" "Willpower" "Edge") 
			"select FinalBody, FinalAgility, FinalStrength, FinalReaction, FinalLogic, FinalCharisma, FinalIntuition, FinalWillpower, FinalEdge from CharacterFinalAttributes where CharacterID=?" id)
	  (create-html-table "Qualities" (list "Quality" "Description")
			"select Quality, Description from CharacterQualities where CharacterID=?" id)
	  (create-html-table "Skills" (list "Skill" "Rating" "Specialisation")
			"select Skill, Rating, Specialisation from CharacterSkills where CharacterID=?" id)
	  (create-html-table "Spells" (list "Spell") "select Spell from CharacterSpells where CharacterID=?" id)
	  (create-html-table "Spirits" (list "Spirit type" "Force" "Services" "Notes") 
			"select SpiritType, Force, Services, Notes from CharacterSpirits where CharacterID=?" id)
	  (create-html-table "Bonded Foci" (list "Type" "Force" "Notes") "select Type, Force, Notes from CharacterBondedFoci where CharacterID=?" id)
	  (create-html-table "Initiation" (list "Initiation grade") "select InitiationGrade from CharacterInitiation where InitiationGrade > 0 and CharacterID=?" id)
	  (create-html-table "Metamagic" (list "Metamagic" "Times taken") "select Metamagic, Taken from CharacterMetamagic where CharacterID=?" id)
	  (create-html-table "Complex Forms" (list "Complex Form" "Rating") 
			"select ComplexForm, Rating from CharacterComplexForms where CharacterID=?" id)
	  (create-html-table "Sprites" (list "Sprite type" "Rating" "Tasks" "Notes")
			"select SpriteType, Rating, Tasks, Notes from CharacterSprites where CharacterID=?" id)
	  (create-html-table "Submersion" (list "Submersion grade") "select SubmersionGrade from CharacterSubmersion where SubmersionGrade > 0 and CharacterID=?" id)
	  (create-html-table "Echoes" (list "Echo" "Times taken") "select Echo, Taken from CharacterEchoes where CharacterID=?" id)))

	 (html-page "Character not found" (list (create-html-paragraph "Sorry, it appears you entered an invalid character URL") (create-html-paragraph (->string hash))))))) 

 
(define (edit-character-html hash) 
 (let 
  ((id (sql-type->string (get-id-from-hash (->string hash) #t))))
    (html-page  
     (get-name id)
     (list 
	(create-html-caption 1 "Edit character")
     	(current-resources id)
     	(create-html-table "" (list "Description") "select Description from Characters where CharacterID=?" id)
     	(create-numeric-value-form id  "Attributes" 
				(map (lambda (n d v) (make-attribute n d v)) 
		(list "BodyAttr" "Agility" "Strength" "Reaction" "Logic" "Charisma" "Intuition" "Willpower" "Edge")
		(list "Body" "Agility" "Strength" "Reaction" "Logic" "Charisma" "Intuition" "Willpower" "Edge")
 		(first-row (open-database database-path) "select FinalBody, FinalAgility, FinalStrength, FinalReaction, FinalLogic, FinalCharisma, FinalIntuition, FinalWillpower, FinalEdge from CharacterFinalAttributes where CharacterID=?" id))
	"attr" "do" )
 	(create-skills-form id "Skills" 
 		(map-row (lambda (n r g) (make-skill n r g)) (open-database database-path) 
 			"select Name, coalesce(Rating, 0), coalesce(SkillGroup, 'none') from (select distinct Skills.Name, Rating, SkillGroup from Skills left outer join ViewCharacterGroupedSkills on Skills.Name = ViewCharacterGroupedSkills.Skill and CharacterID = ? order by Skills.IsActive desc, Skills.SkillGroup asc, Skills.Name asc)" id) 
		(map-row (lambda (row) row) (open-database database-path) "select Name from SkillGroups") "skill" "sdo") 
))))

   
(define list-character-links
   (lambda ()
      (let ((query "select CharacterID from Characters")
            (db (open-database database-path)))
         (string-append
            "<p><ul>"
            (apply string-append 
                (map-row 
                (lambda (row) (string-append "<li>" (create-character-link (sql-type->string row) #f #f)  "</li>"))
                 db
                 query))
             "</ul></p>"))))
        
(define metatype-options-from-db
    (map-row
        (lambda row 
             (let* ((name (sql-type->string (car row)))
                    (bp-cost (sql-type->string (cadr row))))
            (make-option name  (string-append name "(" bp-cost " BP)"))))
        (open-database database-path)
        "select Name, BPCost from Metatypes"))
        
(define character-input-form
    (create-html-form
    "newcharacter"
    (create-html-input "cname" "character name" "")
    (create-html-input "cdesc" "character description" "")
    (create-html-select "metatype" "metatype"
                    metatype-options-from-db)
    (create-html-input "bp" "build points" "400")
    (create-html-input "ckarma" "current karma" "0")
    (create-html-input "tkarma" "total karma" "0")
    (create-html-input "nlang" "native language" "")
    (create-html-submit "action" "Submit")))
          

(define new-character-button 
  "<a href=\"http://sr4.ef.gy/sr4web?page=createcharacter\">Create new character</a>")

(define new-character-html
  (string-append "Content-Type: application/xhtml+xml\n\n<?xml version='1.0' encoding='UTF-8'?><html xmlns='http://www.w3.org/1999/xhtml'><head><title>New Character</title></head>
     <body>"
     character-input-form
     "</body></html>"))
(define (update-character-attributes action attr attvalue cid)
	(letrec ((database (open-database database-path))
                     (update-string (lambda (att action value) (if (or (string=? action "inc") 
	              						 (string=? action "dec")) 
							     (string-append "update CharacterAttributes set " att "=" att (if (string=? action "inc") " + " " - ") "1 ")
							     (string-append "update CharacterAttributes set " att "= ? " ))))
		     (final-string (if (and (string? attr)
					    (string? action)
					    (string? attvalue))
						(string-append (update-string attr action attvalue)
						  " where CharacterID=?")
						"")))
        (cond ((or (not (string? attr))
		  (not (string? action))) #f)
	      ((> (string-length action) 1)
		(execute database final-string
             	cid))
              ((and  (> (string-length attr) 0)
		     (> (string-length attvalue) 0))
                (execute database final-string
                attvalue
                cid)))))

        
(define (has-skill id skill)
   (= 1 (first-result (open-database database-path) "select exists(select CharacterID from ViewCharacterGroupedSkills where CharacterID = ? and Skill = ?)" id skill)))


(define (update-character-skills action skill rating isgroup  cid)
       (letrec ((database (open-database database-path))
	    (maybe-string (lambda (x) (if (string? x) x "")))
	    (saction (maybe-string action))
            (sisgroup (maybe-string isgroup))
 	    (srating (maybe-string rating))
	    (sskill (maybe-string skill))
	    (addend (cond ((string=? saction "inc") " + 1")
			   ((string=? saction "dec") " - 1")
			   (else "")))
            (table (if (string=? sisgroup "1") "CharacterSkillGroups" "CharacterSkills"))
	    (column (if (string=? sisgroup "1") "SkillGroup" "Skill"))
	    (update-table (lambda (set) (string-append "update " table " set Rating = " set " where CharacterID =? and " column "=?")))
	    (sql (cond ((or (string=? saction "inc") (string=? saction "dec"))
			    (if (has-skill (maybe-string cid) sskill) (update-table (string-append "Rating" addend)) 
			      (string-append "insert into " table " (CharacterID, " column ", Rating) values (?, ?, 1)")))
			((string=? saction "") 
			    (update-table srating))
			(else ""))))
			(if  (and (string? skill) (> (string-length skill) 1))
				(execute database sql cid skill) #f)))

(define home-page
  (string-append prefix-string (read-all "./start.xhtml")))


(handle-exceptions exn
  (display 
     (html-page "Exception"
	(list
	 (create-html-paragraph (get-condition-property exn 'exn 'message)))))
;      (create-html-link (get-environment-variable "REQUEST_URI") "none" "Back to previous page"))))
(cond
  ((not (get-environment-variable "NGX_ARG_PAGE")) (display home-page))
  ((irregex-match-data? (irregex-search (sre->irregex "characters") (get-environment-variable "NGX_ARG_PAGE")))
   (cond
      ((string=? (->string (get-environment-variable "NGX_ARG_ADMIN")) "true")
	(display (edit-character-html (get-environment-variable "NGX_ARG_HASH"))))
      ((string=? (->string (get-environment-variable "NGX_ARG_ADMIN")) "false")
	(display (display-character-html (get-environment-variable "NGX_ARG_HASH") "")))	
      (else (display (html-page "Character not found" (list (create-html-paragraph "Sorry, it appears you entered an invalid character URL")))))))
  ((irregex-match-data? (irregex-search (sre->irregex "createcharacter") (get-environment-variable "NGX_ARG_PAGE"))) 
      (display new-character-html))
  ((irregex-match-data? (irregex-search (sre->irregex "newcharacter") (->string (get-environment-variable "REQUEST_URI"))))
      (begin
        (let ((database (open-database database-path)))
          (execute
            database
            (prepare-input "Characters" (list "Name" "Description" "Metatype" "TotalBP" "CurrentKarma" "TotalKarma" "NativeLanguage")) 
            (get-environment-variable "NGX_ARG_CNAME")
	    (get-environment-variable "NGX_ARG_CDESC")
            (get-environment-variable "NGX_ARG_METATYPE")
            (get-environment-variable "NGX_ARG_BP")
            (get-environment-variable "NGX_ARG_CKARMA")
            (get-environment-variable "NGX_ARG_TKARMA")
            (get-environment-variable "NGX_ARG_NLANG"))
          (let* ((id (get-last-rowid "Characters"))
            (hash (message-digest-string (sha256-primitive) (sql-type->string id)))
	     (admin-hash (message-digest-string (sha256-primitive) (->string (time->nanoseconds (current-time)))))
		)
	   (begin
          (execute
            (open-database database-path)
            "update Characters set Hash = ?, AdminHash = ? where CharacterID = ?"
            hash admin-hash (sql-type->string id))
       	(display (display-character-html hash admin-hash)) 
	)))))
  ((irregex-match-data? (irregex-search (sre->irregex "edit-character") (->string (get-environment-variable "NGX_ARG_PAGE")))) 
	(begin 
		(update-character-attributes (get-environment-variable "NGX_ARG_DO") 
		                     (get-environment-variable "NGX_ARG_ATTR")
				     (get-environment-variable "NGX_ARG_ATTVALUE")
				     (get-environment-variable "NGX_ARG_CID"))
		(update-character-skills (get-environment-variable "NGX_ARG_SDO")
					 (get-environment-variable "NGX_ARG_SKILL") #f #f
;					 (get-environment-variable "NGX_ARG_RATING")
;					 (get-environment-variable "NGX_ARG_ISGROUP")
					 (get-environment-variable "NGX_ARG_CID"))
;;		(update-character-qualities (get-environment-variable "NGX_ARG_QUALITY")
;					    (get-environment-variable "NGX_ARG_QRATING")
;				            (get-environment-variable "NGX_ARG_QDESC")
;					    (get-environment-variable "NGX_ARG_QDO"))
               (display (edit-character-html (get-environment-variable "NGX_ARG_CID")))))
  (else (display home-page))))

