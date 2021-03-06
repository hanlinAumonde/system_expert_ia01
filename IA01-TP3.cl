;;-------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------BASE DE REGLE----------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

;;formulaire : (Ri ((premise1) (premise2) ....) (conclusion))

(setq *BDR* '(
              (R11 ((Etat_Matiere plasma) (Forme sphere) (Mode_Lumineux radiation)) (Type etoile))   
              (R12 ((Etat_Matiere degeneree) (Forme sphere) (Reaction_nucleaire nil)) (Type etoile_compacte))
              (R13 ((Vitesse_Rot rapide) (Forme sphere) (Masse (1.35 2.1))) (Type etoile_neutron))
              (R14 ((Masse (> 3.3)) (Forme sphere) (Horizon_evenement t)) (Type trou_noir))
              (R15 ((Masse (> 1000000)) (Forme sphere) (Position centre_galaxie)) (Type trou_noir))
              (R16 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine t)) (Type planete))
              (R17 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type planete_naine))
              (R18 ((Forme  irregulier) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type asteroide))
              (R19 ((Reaction_nucleaire nil) (Orbite_Planete t) (Orbite_Etoile nil) (Effacer_Voisine nil)) (Type satellite))
              
              (R21 ((Type etoile) (Reaction_nucleaire normal)) (Type etoile_sequence_principale))
              (R22 ((Type etoile_compacte) (Masse (0.17 1.33))) (Type nain_blanc))
              (R23 ((Type etoile_compacte) (Masse (1.35 2.1))) (Type etoile_neutron))
              (R24 ((Type etoile) (Reaction_nucleaire pres_de_fin) (Masse (0.3 8))) (Type geante_rouge))
              (R25 ((Etat_Matiere gaz) (Type planete)) (Type planete_jovienne))
              (R26 ((Etat_Matiere solide) (Type planete)) (Type planete_tellurique))             
              (R27 ((Type planete_naine) (Etat_Matiere gaz)) (Type planete_naine_gazeuse))
            
              (R31 ((Type etoile_neutron) (Signal_Impulsion periodique)) (Type pulsar))  
             ))

  ;;valeur possible:
    ;;Etat_matiere?????????????????? : solide(??????) / gaz???????????? / plasma?????????????????? / degeneree???????????????
    ;;Mode_Lumineux?????????????????? : radiation(??????) / nil
    ;;Type???????????? : etoile???????????? / etoile_compacte??????????????? / nain_blanc??????????????? / etoile_neutron???????????????
                    ;;pulsar(?????????) / geante_rouge???????????????/ trou_noir????????????/ planete????????????/ planete_naine_gazeuse?????????????????????
                    ;;planete_jovienne?????????????????? / planete_tellurique??????????????????/ planete_naine???????????????
                    ;;asteroide??????????????? / satellite????????????/ etoile_sequence_principale???????????????
    ;;Reaction_nucleaire??????????????? : normal / nil / pres_de_fin
    ;;Masse : bdf????????????????????????????????????????????????b dr???masse???????????????
    ;;Vitesse_Rot?????????????????? : rapide / normal ???????????????????????????????????????
    ;;Horizon_evenement?????????????????? : t / nil
    ;;Position : centre_galaxie / incertain
    ;;Forme???????????? : sphere / irregulier???????????????
    ;;Orbite_Etoile????????????????????? : t / nil
    ;;Effacer_Voisine?????????????????????????????? :  t / nil
    ;;Orbite_Planete????????????????????? : t / nil
    ;;Signal_Impulsion?????????????????? :  periodique / normal

;;Exemple d'un base de faits
;;????????????????????????
;;(setq *BDF* '(
              ;;(Etat_matiere roche)
              ;;(Forme sphere)
              ;;(Mode_Lumineux nil)
              ;;(Orbite_Etoile t)
              ;;(Orbite_Planete nil)
              ;;(Reaction_nucleaire nil)
              ;;(Masse 3.0e-6)
              ;;(Vitesse_Rot 86400)
              ;;(Effacer_Voisine t)
              ;;(Horizon_evenement nil)
              ;;(Position incertain)
              ;;(Signal_Impulsion nil)
              ;;(Type inconnu) 
              ;;))
;;Masse ??????????????????????????????????????????????????????3.0e-6???0.000003?????????????????????
;;Vitesse_Rot ???????????????????????????????????????????????????????????????????????????????????????24*60*60=86400s???


;;-----------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------initialisation du BDF-----------------------------------------------------
;;-----------------------------------------------------------------------------------------------------------------------

(setq list_champs '(Etat_matiere Forme Mode_Lumineux Orbite_Etoile Orbite_Planete Reaction_nucleaire Masse 
                    Vitesse_Rot Effacer_Voisine Horizon_evenement Position Signal_Impulsion))

(defun init_BDF ()
  (setq *BDF* nil)
  (format t "Veuillez saisir les donnees du corps celeste ???~%")
    (dolist (champ list_champs)
       (init_Champs champ)
      )
  (setq *BDF* (append *BDF* (list (list 'Type 'inconnu))))
  (format t "~%------initialisation finish !------~%")
)

(defun init_Champs (nom_Champ)
   (let ((valeur nil))
      (format t "Quelle est le ~S du corps celeste ?~%" nom_Champ)
      (cond 
        ((equal nom_Champ 'Etat_matiere) 
          (format t "(roche / glace / metal / hydrogene / helium / plasma / degeneree / inconnu)~%")      
        )
        ((equal nom_Champ 'Forme) 
          (format t "(sphere / irregulier / inconnu)~%")
        )
        ((equal nom_Champ 'Mode_Lumineux) 
          (format t "(radiation / nil / inconnu)~%")
        )
        ((equal nom_Champ 'Orbite_Etoile) 
          (format t "(t / nil / inconnu)~%")
        )
        ((equal nom_Champ 'Orbite_Planete) 
          (format t "(t / nil / inconnu)~%")
        )
        ((equal nom_Champ 'Reaction_nucleaire) 
          (format t "(normal / nil / pres_de_fin / inconnu)~%")
        )
        ((equal nom_Champ 'Masse) 
          (format t "(Entrez un nombre ou reel)~%")
        )
        ((equal nom_Champ 'Vitesse_Rot) 
          (format t "(Entrez un nombre ou reel)~%")
        )
        ((equal nom_Champ 'Effacer_Voisine) 
          (format t "(t / nil / inconnu)~%")
        )
        ((equal nom_Champ 'Horizon_evenement) 
          (format t "(t / nil / inconnu)~%")
        )
        ((equal nom_Champ 'Position) 
          (format t "(centre_galaxie / incertain / inconnu)~%")
        )
        ((equal nom_Champ 'Signal_Impulsion) 
          (format t "(periodique / normal / nil / inconnu)~%")
        )
      )
      (setq valeur (read))
      (setq *BDF* (append *BDF* (list (list nom_Champ valeur)))) 
   )
)


;;-------------------------------------------------------------------------------------------------------------------------
;;-------------------------------------------------fonction utile----------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

;;?????????????????????????????????bdf???????????????????????????????????????????????????????????????
(defun Transformation_donnee (bdf)
  (Transform_Matiere bdf)
  (Transform_Masse bdf)
  (Transform_Vitesse_Rot bdf)
  (format t "~%------Transformation finish !------~%~%")
)

(defun Transform_Matiere (bdf)
  (let ((matiere (cadr (assoc 'Etat_Matiere bdf))))
     (cond 
       ((or
          (equal matiere 'roche)
          (equal matiere 'glace)
          (equal matiere 'metal)
        )
        (setf (nth 1 (nth 0 bdf)) 'solide))
       ((or 
          (equal matiere 'hydrogene)
          (equal matiere 'helium)
          )
        (setf (nth 1 (nth 0 bdf)) 'gaz)) 
       ((and
         (not (equal matiere 'plasma))
         (not (equal matiere 'degeneree))
         ) 
        (setf (nth 1 (nth 0 bdf)) 'inconnu))
     )
  )    
)


(defun Transform_Masse (bdf)
  (let ((masse (cadr (assoc 'Masse bdf))) (list_interval_masse nil))
       (if (not (or (floatp masse) (numberp masse)))
          (progn
            (setf (nth 1 (nth 6 bdf)) nil)
            (return-from Transform_Masse nil)))  
       (if (and (> masse 0.17) (< masse 1.33)) 
         (setq list_interval_masse (append list_interval_masse (list (list 0.17 1.33))))
       )
       (if (and (> masse 1.35) (< masse 2.1))
         (setq list_interval_masse (append list_interval_masse (list (list 1.35 2.1))))
       )
       (if (and (> masse 0.3) (< masse 8)) 
         (setq list_interval_masse (append list_interval_masse (list (list 0.3 8))))
       )
       (if (> masse 3.3)
         (setq list_interval_masse (append list_interval_masse (list (list '> 3.3))))
       )
       (if (> masse 1000000)
         (setq list_interval_masse (append list_interval_masse (list (list '> 1000000))))
         )
       (setf (nth 1 (nth 6 bdf)) list_interval_masse)
   )
)

(defun Transform_Vitesse_Rot (bdf)
  (let ((vitesse (cadr (assoc 'Vitesse_Rot bdf))))
    (if (or (floatp vitesse) (numberp vitesse))
       (if (<= vitesse 30)
           (setf (nth 1 (nth 7 bdf)) 'rapide)
         (setf (nth 1 (nth 7 bdf)) 'normal)
       )
      (setf (nth 1 (nth 7 bdf)) 'inconnu)
    )
  )    
)


;;------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------moteur d'inference--------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------

(defun moteur_inference (choix_chainage bdf bdr)
  (cond 
   ((equal choix_chainage 'avant) 
      (avant_Profondeur bdf bdr)  
   )
   ((equal choix_chainage 'arriere)
      (format t "~%~% Veuillez saisir le type de corps c??leste que vous souhaitez confirmer :~%~%")
      (setq type_celeste_BUT (read))
      (arriere_Largeur type_celeste_BUT bdf bdr)
   )
   )
)


;;?????????????????????????????????bdf??????????????????????????????????????????regle??????
(defun regleSuffisant (bdf bdr)
  (let ((regleSuff nil))
    (dolist (regle bdr)
       (let ((verifier t))
         (dolist (champ_regle (cadr regle))
             (cond
               ((equal (car champ_regle) 'Masse) 
                 (if (member (cadr champ_regle) (cadr (assoc (car champ_regle) bdf)) : test 'equal)
                   nil (setq verifier nil)))
               (t 
                 (if (member champ_regle bdf : test 'equal) 
                   nil (setq verifier nil)))
             )
         )
         (if (and 
              (equal verifier t) 
              (or
                 (member (nth 12 bdf) (cadr regle) : test 'equal)
                 (equal (nth 1 (nth 12 bdf)) 'inconnu)
              ))
              (push (car regle) regleSuff))
       )
    )
    (reverse regleSuff)
  )
)

;;?????????????????????????????????????????????regle???????????????????????????????????????????????????
;;????????????bdf?????????????????????????????????????????????type???????????????type?????????,??????????????????????????????????????????????????????????????????????????????
;;??????????????????????????????????????????

(setq type_non_compatible '((etoile etoile_neutron) (etoile trou_noir) (etoile planete) (etoile planete_naine) (etoile satellite)
                            (etoile_neutron planete) (etoile_neutron planete_naine) (etoile_neutron satellite)
                            (trou_noir planete_naine) (trou_noir planete) (trou_noir satellite)
                           )
                      
(defun verifier_compatible (bdf bdr)
    (let ((reglesuff_1er (regleSuffisant bdf bdr)) (list_type nil) (res t))
       (dolist (regle reglesuff_1er)
          (setq list_type (append list_type (list (cadr (caddr (assoc regle bdr))))))
       )
       (dolist (champ type_non_compatible)
          (if (and 
                (member (car champ) list_type) 
                (member (cadr champ) list_type))
             (setq res nil)      
          )
       )
       res
    )
)


;;1er cas : chainage avant en profondeur d'abord

;;???????????????????????????regle
(defun appliquer_regle (regle bdr bdf)
  (if (equal (nth 1 (nth 12 bdf)) 'inconnu) 
      (format t "~%----------------------------------------------------------------------------------~%"))
  (if (member regle (regleSuffisant bdf bdr)) 
    (let ((regleComplet (assoc regle bdr)))
      (format t "~%Type ancienne : ~S ~% les conditions ~S appliquent le regle ~S ~%Type maintenant : ~S" (nth 1 (nth 12 bdf)) (cadr regleComplet) regle (cadr (caddr regleComplet)))
      (setf (nth 1 (nth 12 bdf)) (cadr (caddr regleComplet)))
      )
  )
  (format t "~%")
)

(defun avant_Profondeur1 (bdf bdr regleOld)
  (let ((regleS (regleSuffisant bdf bdr)) 
        (retourner nil))
    (cond 
      ((not (null regleS))
          (while (and (not retourner) regleS)
            (let ((type_copy (nth 1 (nth 12 bdf))))  
              (push (car regleS) regleOld)
              (appliquer_regle (car regleS) bdr bdf)
              (setq retourner (avant_Profondeur bdf bdr regleOld))
              (if retourner (format t "~%Retourner avant d'appliquer ~S ~%" (pop regleS)))
              (setf (nth 1 (nth 12 bdf)) type_copy)
              (if (not regleS) 
                   (return-from avant_Profondeur t)
                (progn 
                  (pop regleOld)
                  (setq retourner nil)  
                  )
              )
            )
       ))
      ((and (null regleOld) (null regleS))
           (format t "~%Erreur ! Donnees insuffisantes ou erreur !~%")
      )
      (t
        (format t "~%Le raisonnement est termine, le resultat du raisonnement est TYPE == ~S" (nth 1 (nth 12 bdf)))
        (format t "~%Les regles utilise sont : ")
        (print (reverse regleOld))
        t
      )
    )
  )
)

(defun avant_Profondeur (bdf bdr)
   (if (equal (verifier_compatible bdf bdr) t)
       (avant_Profondeur1 bdf bdr nil)
      (format t "~%Erreur ! Donnee invalide !~%")
   )
)

;;2eme cas : chainage arriere en largeur d'abord

  ;;???????????????????????????????????????type_but???regle?????????????????????????????????regle???????????????????????????????????????
  (defun inference_inverse (type_but bdr)
      (let ((list_conditions nil))
        (dolist (x bdr) 
           (if (equal (cadr (caddr x)) type_but) 
               (setq list_conditions (append list_conditions (list (cadr x))))
           )
        )
        list_conditions
      )  
  )


;;???????????????????????????????????????list????????? ( ((...) (...))  ((...) (...))  ((...) (...)) ) ?????????
(defun flatten (L) (if L (append (car L)(flatten (cdr L)))))

;;????????????????????????????????????????????????list_conds???????????????
;;????????????list_conds???((Type ?????????)) , ????????????(Cond1 Cond2) , (Cond3 Cond4)????????????????????????list_conds,
;;?????????????????????result???((cond1 cond2) (cond3 cond4))
;;????????????list_conds???((Type ?????????) cond5),???result???((cond5 cond1 cond2) (cond5 cond3 cond4))
(defun successeur (list_conds bdr)
   (let ((result nil) (conds_sans_type nil)
         (champ_type (assoc 'Type list_conds))
         )
     (setq conds_sans_type (diff list_conds (list champ_type)))
     (cond
       ((equal conds_sans_type nil)
          (dolist (x (inference_inverse (cadr champ_type) bdr))
             (setq result (append result (list x)))
          )
       )
       ((not (null champ_type))
          (dolist (x (inference_inverse (cadr champ_type) bdr))
             (setq result (append result (list (append conds_sans_type x))))
          )
       )
     )
   result
   )
)

;;?????????????????????bdf???????????????????????????????????????
(defun verifier_conditions (list_conditions bdf)
   (let ((res t) (mass (cadr (assoc 'Masse bdf))))
   (dolist (x list_conditions)
      (cond 
        ((equal (car x) 'Masse) 
           (if (not (member (cadr x) mass : test 'equal)) 
               (setq res nil)
           ))
        ((not (member x bdf : test 'equal)) (setq res nil))
      )
   )
   res)
)

(defun arriere_Largeur1 (list_cadidates bdf bdr)
     (if (null list_cadidates) 
         (format t "~%On ne peut pas verifier le type BUT~%")
       (format t "~%On a les conditions maintenant :~%"))
     (dolist (x list_cadidates)
        (format t "~S ~%-------------------------------------------------------~%" x)   ;;?????????????????????????????????conditions
        (if (equal (verifier_conditions x bdf) t) 
            (progn
              (format t "Conditions verifiees avec succes !~%~%")
              (return-from arriere_Largeur1 nil)
            ) ;;???bdf??????????????????????????????????????????????????????????????????????????????
          (format t "et on n'a pas encore verifier les conditions dans le base de faits~%~%") ;;???????????????????????????????????????
        )
     )
     (format t "~%**************************************************************~%")
   (cond 
     ((not list_cadidates) nil)
     (t (arriere_Largeur1 (flatten (mapcar #'(lambda(xx) (successeur xx bdr)) list_cadidates))  bdf bdr)
     )
   )
)

(defun arriere_Largeur (type_but bdf bdr)
  (if (equal (verifier_compatible bdf bdr) t)
      (arriere_Largeur1 (list (list (list 'Type type_but))) bdf bdr)
    (format t "~%Erreur ! Donnee invalide !~%")
  )
)


;;-----------------------------------------------------------------------------------------------------------------------
;;------------------------------fonction client--------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------------------------------------------
(defun corps_celeste ()   
   (format t "~%******************************************programme commencer*********************************************~%")
   (let ((programme 0))
    (while (equal programme 0)  
     (init_BDF)
     (Transformation_donnee *BDF*)
     (let ((autrechoix 0) (votreChoix nil)) 
      (while (equal autrechoix 0) 
       (format t "Entrez votre choix de la chainage (avant ou arriere) :~%")
       (setq votreChoix (read))
       (moteur_inference votreChoix *BDF* *BDR*)
       (format t "~%Voulez-vous appliquer l'autre chainage sur ce BDF (oui : 0 / non : 1)?~%")  
       (setq autrechoix (read))
      )
     )  
     (format t "~%Voulez-vous quitter le programme ? (Entrez 1 pour quitter , entrez 0 pour continuer)~%")
     (setq programme (read))
    )
   )
     (format t "~%******************************************programme terminer*********************************************~%")
)
