;;-------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------BASE DE REGLE----------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

;;formulaire : (Ri ((premise1) (premise2) ....) (conclusion))

(setq *BDR* '(
              (R1 ((Etat_Matiere plasma) (Forme sphere) (Mode_Lumineux radiation)) (Type etoile))   
              (R2 ((Type Etoile) (Reaction_nucleaire normal)) (Type etoile_sequence_principale))
              (R3 ((Etat_Matiere degeneree) (Reaction_nucleaire nil)) (Type etoile_compacte))
              (R4 ((Type etoile_compacte) (Masse (0.17 1.33))) (Type nain_blanc))
              (R5 ((Type etoile_compacte) (Masse (1.35 2.1))) (Type etoile_neutron))
              (R6 ((Vitesse_Rot rapide) (Masse (1.35 2.1))) (Type etoile_neutron))
              (R7 ((Type etoile_neutron) (Signal_Impulsion periodique)) (Type pulsar))
              (R8 ((Type etoile) (Reaction_nucleaire pres_de_fin) (Masse (0.3 8))) (Type geante_rouge))
              (R9 ((Masse (> 3.3)) (Horizon_evenement t)) (Type trou_noir))
              (R10 ((Masse (> 1000000)) (Position centre_galaxie)) (Type trou_noir))
              (R11 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine t)) (Type planete))
              (R12 ((Etat_Matiere gaz) (Type planete)) (Type planete_jovienne))
              (R13 ((Etat_Matiere solide) (Type planete)) (Type planete_tellurique))
              (R14 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type planete_naine))
              (R15 ((Type planete_naine) (Etat_Matiere gaz)) (Type planete_naine_gazeuse))
              (R16 ((Forme incertain) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type asteroide))
              (R17 ((Reaction_nucleaire nil) (Orbite_Planete t) (Orbite_Etoile nil) (Effacer_Voisine nil)) (Type satellite))
))
  ;;valeur possible:
    ;;Etat_matiere（物质状态） : solide(固态) / gaz（气态） / plasma（等离子体） / degeneree（简并态）
    ;;Mode_Lumineux（发光模式） : radiation(辐射) / nil
    ;;Type（类型） : etoile（恒星） / etoile_compacte（致密星） / nain_blanc（白矮星） / etoile_neutron（中子星）
                    ;;pulsar(脉冲星) / geante_rouge（红巨星）/ trou_noir（黑洞）/ planete（行星）/ planete_naine_gazeuse（气态矮行星）
                    ;;planete_jovienne（类木行星） / planete_tellurique（类地行星）/ planete_naine（矮行星）
                    ;;asteroide（小行星） / satellite（卫星）/ etoile_sequence_principale（主序星）
    ;;Reaction_nucleaire（核反应） : normal / nil / pres_de_fin
    ;;Masse : bdf里为数值形式，通过特定函数转化为b dr中masse的描述形式
    ;;Vitesse_Rot（自转速度） : rapide / normal （同上，将数值转化为描述）
    ;;Horizon_evenement（事件视界） : t / nil
    ;;Position : centre_galaxie / incertain
    ;;Forme（形状） : sphere / irregulier（不规则）
    ;;Orbite_Etoile（绕恒星运行） : t / nil
    ;;Effacer_Voisine（清空轨道附近区域） :  t / nil
    ;;Orbite_Planete（绕行星运行） : t / nil

;;Exemple d'un base de faits
;;以地球的数据为例
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
              ;;(Type inconnu) 
              ;;))
;;Masse 数据以太阳为标准，如上例中地球质量为3.0e-6（0.000003）倍的太阳质量
;;Vitesse_Rot 数据单位为秒，即天体自转一圈所需的时间，如上例中地球自转（24*60*60=86400s）


;;-----------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------initialisation du BDF-----------------------------------------------------
;;-----------------------------------------------------------------------------------------------------------------------

(setq list_champs '(Etat_matiere Forme Mode_Lumineux Orbite_Etoile Orbite_Planete Reaction_nucleaire Masse Vitesse_Rot Horizon_evenement Position))

(setq *BDF* nil)

(defun init_BDF ()
  (format t "Veuillez saisir les donnees du corps celeste ：~%")
    (dolist (champ list_champs)
       (init_Champs champ)
      )
  (setq *BDF* (append *BDF* (list (list 'Type 'inconnu))))
  (format t "initialisation finish !")
)

(defun init_Champs (nom_Champ)
   (let ((valeur nil))
     (format t "Quelle est le ~S materielle du corps celeste ?~%" nom_Champ)
     (setq valeur (read))
     (setq *BDF* (append *BDF* (list (list nom_Champ valeur))))
   )
)


;;-------------------------------------------------------------------------------------------------------------------------
;;-------------------------------------------------fonction utile----------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

;;以下几个函数用于将输入bdf的具体数据转化成可以由推理机识别的具体描述
(defun Transformation_donnee (bdf)
  (Transform_Matiere bdf)
  (Transform_Masse bdf)
  (Transform_Vitesse_Rot bdf)
  (format t "Transformation finish")
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
      )
  )    
)

(defun Transform_Masse (bdf)
   (let ((masse (cadr (assoc 'Masse bdf))) (list_interval_masse nil))
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
       (if (<= vitesse 30)
           (setf (nth 1 (nth 7 bdf)) 'rapide)
         (setf (nth 1 (nth 7 bdf)) 'normal)
       )
  )    
)

;;此函数用于寻找对于当前bdf来说可以用于进行下一步推理的regle列表
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
                 (member (nth 11 bdf) (cadr regle) : test 'equal)
                 (equal (nth 1 (nth 11 bdf)) 'inconnu)
              ))
              (push (car regle) regleSuff))
       )
    )
    (reverse regleSUff)
  )
)

;;此函数用于执行regle
(defun appliquer_regle (regle bdr bdf)
  (if (equal (nth 1 (nth 11 bdf)) 'inconnu) 
      (format t "~%----------------------------------------------------------------------------------~%"))
  (if (member regle (regleSuffisant bdf bdr)) 
    (let ((regleComplet (assoc regle bdr)))
      (format t "~%Type ancienne : ~S ~% les conditions ~S appliquent le regle ~S ~%Type maintenant : ~S" (nth 1 (nth 11 bdf)) (cadr regleComplet) regle (cadr (caddr regleComplet)))
      (setf (nth 1 (nth 11 bdf)) (cadr (caddr regleComplet)))
      )
  )
  (format t "~%")
)


;;------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------moteur d'inference--------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------

(defun moteur_inference (choix_chainage bdf bdr)
  (cond 
   ((equal choix_chainage 'avant) 
      (avant_Profondeur bdf bdr nil)  
      )
   )
)

;;1er cas : chainage avant en profondeur d'abord
(defun avant_Profondeur (bdf bdr regleOld)
  (let ((regleS (regleSuffisant bdf bdr)) 
        (retourner nil))
    (cond 
      ((not (null regleS))
          (push (car regleS) regleOld)
       (while (and (not retourner) regleS)
            (let ((type_copy (nth 1 (nth 11 bdf))))  
              (appliquer_regle (car regleS) bdr bdf)
              (setq retourner (avant_Profondeur bdf bdr regleOld))
              (if retourner (format t "~%Retourner avant d'appliquer ~S ~%" (pop regleS)))
              (setf (nth 1 (nth 11 bdf)) type_copy)
              (if (not regleS) 
                   (return-from avant_Profondeur t)
                (setq retourner nil)  
              )
            )
       ))
      (t
       (format t "~%Le raisonnement est termine, le resultat du raisonnement est TYPE == ~S" (nth 1 (nth 11 bdf)))
       (format t "~%Les regles utilise sont : ")
       (print (reverse regleOld))
       t
       )
    )
 ))

;;2eme cas : chainage arriere en profondeur d'abord

