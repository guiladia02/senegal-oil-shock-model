# Analyse économique du Sénégal : exposition aux chocs pétroliers

## Présentation du projet

Ce projet étudie l’exposition de l’économie sénégalaise aux variations du prix des produits pétroliers à partir des matrices de comptabilité sociale (MCS) de 2014 à 2022. L’objectif est d’identifier les secteurs les plus exposés, d’analyser les interdépendances productives et de simuler les conséquences potentielles d’une hausse des coûts pétroliers.

L’analyse est réalisée avec **R**, à travers le traitement des données, le calcul d’indicateurs sectoriels, la construction de matrices de coefficients techniques et la simulation de scénarios de chocs. Un modèle d’équilibre général calculable sous **GAMS** est envisagé pour approfondir l’étude des effets économiques.

## Objectifs

- Analyser la structure des flux économiques du Sénégal entre 2014 et 2022.
- Identifier les secteurs les plus exposés aux produits pétroliers.
- Étudier l’évolution de cette exposition dans le temps.
- Simuler des scénarios de hausse des coûts de 10 % et 25 %.
- Explorer la propagation directe et indirecte des chocs entre secteurs.
- Développer un modèle économique sous GAMS pour étudier les ajustements de l’économie.

## Données

Le projet repose sur les matrices de comptabilité sociale du Sénégal pour les années 2014 à 2022. Ces matrices décrivent les flux entre les activités productives, les produits, les facteurs de production et les institutions économiques.

Les données ont été importées, nettoyées et structurées avec R. Un contrôle comptable de la matrice 2022 a notamment montré que les 65 comptes communs présentaient des totaux de lignes et de colonnes identiques. Une différence de nomenclature entre les comptes « Accu » et « Epargne » reste à traiter selon les conventions comptables de la source.

## Méthodologie

### 1. Préparation et contrôle des données

- Importation des matrices annuelles avec R.
- Nettoyage et harmonisation des données numériques.
- Contrôle des dimensions et des comptes.
- Vérification de l’équilibre comptable des comptes communs.

### 2. Analyse de l’exposition sectorielle

Un indicateur d’exposition est calculé en rapportant les achats du produit pétrolier étudié à la production de chaque activité. Cet indicateur permet de comparer la place de ces achats dans les différents secteurs.

Pour 2022, les secteurs présentant les ratios les plus élevés sont :

| Activité | Achats de produits pétroliers / production |
|---|---:|
| Électricité, gaz et activités énergétiques connexes | 62,64 % |
| Transport | 16,48 % |
| Pêche | 15,26 % |
| Ciment et matériaux | 14,76 % |
| Construction | 7,33 % |

Ces ratios sont des indicateurs comptables d’exposition et non des mesures directes de dépendance physique ou de part exacte du coût énergétique.

### 3. Simulation de scénarios de hausse des coûts

Deux scénarios de hausse du prix des produits pétroliers sont étudiés :

- **Scénario 1 : hausse de 10 %**
- **Scénario 2 : hausse de 25 %**

Sous l’hypothèse de quantités achetées constantes et d’une transmission intégrale de la hausse aux coûts d’achat, le surcoût direct agrégé calculé pour 2022 atteint environ :

- **161 132 unités monétaires** pour une hausse de 10 % ;
- **402 830 unités monétaires** pour une hausse de 25 %.

Ces montants sont calculés à partir des achats du produit étudié dans la matrice. Ils représentent des scénarios de surcoût direct, et non des estimations de l’inflation ou de la perte de PIB.

### 4. Analyse des interdépendances productives

Une matrice de coefficients techniques est construite afin d’examiner les relations entre les achats intermédiaires et la production des activités.

Une simulation exploratoire met en évidence des effets potentiels de propagation entre secteurs. Dans le scénario de hausse de 10 %, les effets calculés sont notamment plus élevés pour l’activité énergétique, puis pour les activités liées au ciment et aux matériaux, au transport et à la construction.

Ces résultats dépendent de la formulation retenue pour la propagation des coûts. Ils doivent être interprétés comme des résultats exploratoires et non comme des estimations définitives d’un équilibre économique.

## Principaux résultats

- L’activité énergétique présente le ratio d’exposition le plus élevé en 2022, à **62,64 %**.
- Les ratios calculés pour le transport, la pêche et le secteur du ciment et des matériaux se situent entre **14,76 % et 16,48 %**.
- Entre 2014 et 2022, le ratio d’exposition de l’activité énergétique passe de **64,92 % à 62,64 %**, soit une baisse de **2,28 points de pourcentage**.
- Le surcoût direct simulé en 2022 augmente proportionnellement au choc de prix : le scénario à 25 % génère un surcoût 2,5 fois supérieur à celui du scénario à 10 %.
- Les simulations soulignent l’intérêt d’étudier les liens intersectoriels pour mieux comprendre la transmission potentielle des chocs pétroliers dans l’économie.

## Outils et technologies

- **R :** préparation des données, calcul matriciel, analyse statistique et visualisation.
- **GAMS :** développement prévu d’un modèle d’équilibre général calculable.
- **Git et GitHub :** versionnement du code et documentation du projet.

## Développements prévus

La prochaine étape consiste à construire un modèle d’équilibre général calculable (MEGC) sous GAMS à partir de la MCS de référence. Le modèle devra être calibré et testé avant de permettre une analyse plus complète des effets d’un choc pétrolier sur la production, les revenus, la consommation des ménages et les échanges extérieurs.

L’utilisation de Stata ou d’EViews pourra être envisagée ultérieurement pour des analyses économétriques complémentaires, sous réserve de disposer de séries temporelles adaptées.

## Limites de l’étude

Les simulations actuelles reposent sur des hypothèses simplificatrices concernant les quantités achetées et la transmission des coûts. Elles ne représentent pas encore les réactions des entreprises, les substitutions entre facteurs de production, les changements de comportement des ménages ni l’ensemble des ajustements macroéconomiques.

Les résultats doivent donc être considérés comme exploratoires. Leur interprétation économique sera approfondie après validation des conventions comptables et développement du modèle GAMS.

## Auteur

**Guila DIA**  
Mathématiques appliquées | Data Science | Statistiques et modélisation économique
