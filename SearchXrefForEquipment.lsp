(vl-load-com)

(defun c:GetEquipmentTag ( / ent obj tag)
  (and (setq ent (car (entsel "\nSelect an ACPPEQUIPMENT: ")))
       (setq obj (vlax-ename->vla-object ent))
       (vlax-property-available-p obj 'Tag) ; Adjust 'Tag if it's named differently
       (setq tag (vlax-get-property obj 'Tag))
       (princ (strcat "\nEquipment Tag: " tag))
       )
  (princ)
)

(defun is-string? (value)
  (if (equal (type value) 'STR) T nil)
)

(defun get-equipment-nozzle-position (entity-data)
  (setq found-port nil)
  (foreach pair entity-data
    (if (and (not found-port) (is-string? (cdr pair)) (wcmatch (cdr pair) "*Port*"))
      (setq found-port T)
    )
    (if (and found-port (= (car pair) 10))
      (return (cdr pair))
    )
  )
)


(defun c:SearchXrefForEquipmentTag (/ xrefs blk blkname ent found-equip minpt maxpt)
  (vl-load-com)
  
  ;; Get a list of all Xrefs (block insertions) in the drawing
  (setq xrefs (ssget "X" '((0 . "INSERT"))))
  
  (if (not xrefs)
    (progn
      (princ "\nNo Xrefs found in the drawing.")
      (exit)
    )
  )
  
  ;; Ask user for the tag to find
  (setq tag-to-find (getstring "\nEnter equipment tag to find: "))
  
  ;; Iterate through each Xref
  (repeat (sslength xrefs)
    (setq blk (ssname xrefs 0))
    ;; Get the block name, which corresponds to the Xref name
    (setq blkname (cdr (assoc 2 (entget blk))))
    
    ;; Print the name of the Xref for debugging
    (princ (strcat "\nChecking Xref: " blkname))
    
    ;; Get the first entity in the Xref's block definition
    (setq ent (cdr (assoc -2 (tblsearch "BLOCK" blkname))))

    
    ;; Loop through all entities in the block definition looking for the equipment
    (while ent
      (if (and (= (cdr (assoc 0 (entget ent))) "ACPPEQUIPMENT")
               (wcmatch (vlax-get-property (vlax-ename->vla-object ent) 'Tag) tag-to-find))
        (setq found-equip ent)
      )
      (setq ent (entnext ent))
    )
    
    ;; If found, get the bounding box and zoom to the equipment
    (if found-equip
      (progn
        (vlax-invoke-method (vlax-ename->vla-object found-equip) 'GetBoundingBox 'minpt 'maxpt)
        (setq minpt (vlax-safearray->list minpt))
        (setq maxpt (vlax-safearray->list maxpt))
        (command "_.ZOOM" "_WINDOW" minpt maxpt)
        (exit)
      )
    )
    
    ;; Move to the next Xref in the list
    (setq xrefs (ssdel blk xrefs))
  )

  (if (not found-equip)
    (princ (strcat "\nEquipment with tag " tag-to-find " not found in any Xref."))
  )

  (princ)
)










(defun remove-duplicates (lst)
  (if (not lst)
      nil
      (cons (car lst) (remove-duplicates (vl-remove (car lst) (cdr lst))))
  )
)

(defun c:ListXrefEntityTypes (/ xrefs blk blkname ent alltypes unique-types entity type)
  ;; Get a list of all Xrefs (block insertions) in the drawing
  (setq xrefs (ssget "X" '((0 . "INSERT")))
        alltypes nil)

  (if (not xrefs)
    (progn
      (princ "\nNo Xrefs found in the drawing.")
      (exit)
    )
  )

  ;; Iterate through each Xref
  (repeat (sslength xrefs)
    (setq blk (ssname xrefs 0))
    ;; Get the block name, which corresponds to the Xref name
    (setq blkname (cdr (assoc 2 (entget blk))))
    
    ;; Get the first entity in the Xref's block definition
    (setq ent (entnext (cdr (assoc -2 (tblsearch "BLOCK" blkname)))))
    
    ;; Loop through all entities in the block definition
    (while ent
      (setq type (cdr (assoc 0 (entget ent))))
      (setq alltypes (cons type alltypes))
      (setq ent (entnext ent))
    )

    ;; Move to the next Xref in the list
    (setq xrefs (ssdel blk xrefs))
  )

  ;; Get unique entity types
  (setq unique-types (remove-duplicates alltypes))

  ;; Print the unique entity types
  (princ "\nEntity types inside Xrefs:")
  (foreach type unique-types
    (princ (strcat "\n- " type))
  )
  (princ)
)

(defun remove-duplicates (lst)
  (if (not lst)
      nil
      (cons (car lst) (remove-duplicates (vl-remove (car lst) (cdr lst))))
  )
)

