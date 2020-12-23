;;BASE DE REGLE

(setq *BDR* '(
              (R1 ((Etat_Matiere plasma) (Forme sphere) (Mode_Lumineux radiation)) (Type etoile))   
              (R2 ((Type Etoile) (Reaction_nucleaire normal)) (Type etoile_sequence_principale))
              (R3 ((Etat_Matiere degeneree) (Reaction_nucleaire nil)) (Type etoile_compacte))
              (R4 ((Type etoile_compacte) (Masse (0.17 1.33))) (Type nain_blanc))
              (R5 ((Type etoile_compacte) (Masse (1.35 2.1)) (Vitesse_Rot rapide)) (Type etoile_neutron))
              (R6 ((Type etoile_neutron) (Signal_Impulsion periodique)) (Type pulsar))
              (R7 ((Type etoile) (Reaction_nucleaire pres_de_fin) (Masse (0.3 8))) (Type geante_rouge))
              (R8 ((Masse (> 3.3)) (Horizon_evenement t)) (Type trou_noir))
              (R9 ((Masse (> 1000000)) (Position centre_galaxie)) (Type trou_noir))
              (R10 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine t)) (Type planete))
              (R11 ((Etat_Matiere gaz) (Type planete)) (Type planete_jovienne))
              (R12 ((Etat_Matiere solide) (Type planete)) (Type planete_tellurique))
              (R13 ((Forme sphere) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type planete_naine))
              (R14 ((Type planete_naine) (Etat_Matiere gaz)) (Type planete_naine_gazeuse))
              (R15 ((Forme incertain) (Reaction_nucleaire nil) (Orbite_Etoile t) (Effacer_Voisine nil)) (Type asteroide))
              (R16 ((Reaction_nucleaire nil) (Orbite_Planete t) (Orbite_Etoile nil) (Effacer_Voisine nil)) (Type satellite))
))
  ;;valeur possible:
    ;;Etat_matiere������״̬�� : solide(��̬) / gaz����̬�� / plasma���������壩 / degeneree����̬��
    ;;Mode_Lumineux������ģʽ�� : radiation(����) / nil
    ;;Type�����ͣ� : etoile�����ǣ� / etoile_compacte�������ǣ� / nain_blanc���װ��ǣ� / etoile_neutron�������ǣ�
                     pulsar(������) / geante_rouge������ǣ�/ trou_noir���ڶ���/ planete�����ǣ�/ planete_naine_gazeuse����̬�����ǣ�
                     planete_jovienne����ľ���ǣ� / planete_tellurique��������ǣ�/ planete_naine�������ǣ�
                     asteroide��С���ǣ� / satellite�����ǣ�/ etoile_sequence_principale�������ǣ�
    ;;Reaction_nucleaire���˷�Ӧ�� : normal / nil / pres_de_fin
    ;;Masse : bdf��Ϊ��ֵ��ʽ��ͨ���ض�����ת��Ϊb dr��masse��������ʽ
    ;;Vitesse_Rot����ת�ٶȣ� : rapide / normal ��ͬ�ϣ�����ֵת��Ϊ������
    ;;Horizon_evenement���¼��ӽ磩 : t / nil
    ;;Position : centre_galaxie / incertain
    ;;Forme����״�� : sphere / irregulier��������
    ;;Orbite_Etoile���ƺ������У� : t / nil
    ;;Effacer_Voisine����չ���������� :  t / nil
    ;;Orbite_Planete�����������У� : t / nil

;;Base de faits
;;�Ե��������Ϊ��
(setq *BDF* '(
              (Etat_matiere roche)
              (Forme sphere)
              (Mode_Lumineux nil)
              (Orbite_Etoile t)
              (Orbite_Planete nil)
              (Reaction_nucleaire nil)
              (Masse 3.0e-6)
              (Vitesse_Rot 86400)
              (Effacer_Voisine t)
              (Horizon_evenement nil)
              (Position incertain)
              ))
;;Masse ������̫��Ϊ��׼���������е�������Ϊ3.0e-6��0.000003������̫������
;;Vitesse_Rot ���ݵ�λΪ�룬��������תһȦ�����ʱ�䣬�������е�����ת��24*60*60=86400s��


;;--------------------------------------------
;;-----------fonction utile-------------------
;;--------------------------------------------

(defun Transformation_donnee (bdf)
  (Transform_Matiere bdf)
  (Transform_Masse bdf)
  (Transform_Vitesse_Rot bdf)
)

(defun )