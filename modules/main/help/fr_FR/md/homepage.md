<img src="banner_homepage.png" alt="banner" style="display:block; margin-left:auto; margin-right:auto;" />

# Nelson 2.0.0

**Nelson 2.0 est une version majeure.** Elle apporte un moteur de langage plus rapide fondé sur du bytecode, la programmation orientée objet complète avec `classdef`, NFlow (un éditeur visuel de schémas-blocs avec simulation native et génération de code C/Rust), un bureau web disponible en aperçu (avec une version WebAssembly qui s'exécute dans le navigateur), ainsi qu'un ensemble bien plus large de types de données, de graphiques, de statistiques et d'outils de développement.

La page d'accueil principale de Nelson se trouve à [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/). La liste complète des changements est disponible dans le [Journal des modifications v2.x.x](CHANGELOG.md).

## Introduction

Nelson est un langage open source de calcul numérique destiné à l'ingénierie, au calcul scientifique et à l'enseignement. Il fournit plus de 3 000 fonctions, natives ou écrites en Nelson, pour le calcul sur tableaux, l'algèbre linéaire, le traitement du signal, l'analyse de données, la visualisation et les interfaces avec des langages externes.

Nelson reprend les conventions orientées tableaux utilisées par les environnements de calcul numérique établis et GNU Octave, tout en conservant son propre système de modules, ses formats de fichiers, ses interfaces moteur et son modèle d'exécution.

## Nouveautés de Nelson 2.0

### Moteur de langage

- **Machine virtuelle à bytecode** : le code est compilé en bytecode au lieu d'être interprété ligne par ligne. Les boucles et les fonctions récursives s'exécutent nettement plus vite.
- **Structure de projet** : paquets, `import`, fonctions imbriquées, fonctions locales dans les scripts et `localfunctions`.
- **Validation des arguments** : blocs `arguments` avec sections `Repeating`, arguments nom-valeur et spécifications de taille N-D, ainsi que `validateattributes`, `validatestring` et `inputParser`.
- **Expressions plus courtes** : un appel peut être suivi directement d'un accès à un champ ou d'une indexation, par exemple `f().Field`, `f()(1)`, `f(){1}`.
- **Syntaxe plus stricte** : `end` est obligatoire pour fermer les blocs, et une incohérence entre nom de fichier et nom de fonction est signalée comme une erreur.
- **Fonctions plus rapides** : `diff`, `interp1`, `interp2`, `interp3`, `filter` et `upfirdn` sont désormais implémentées en C++, et les opérateurs binaires prennent en charge l'expansion implicite de tailles compatibles sur les tableaux N-D, creux et vides.

### Programmation orientée objet avec `classdef`

- **Classes valeur et handle** avec héritage simple et multiple, paquets, membres abstraits et scellés, membres statiques, constantes, énumérations et système d'événements/écouteurs.
- **Validation des propriétés** avec vérification de type et règles de validation, plus `Dependent`, `Observable`, `Transient`, `AbortSet` et `SetAccess = immutable`.
- **Propriétés dynamiques** (`addprop`/`rmprop`), références faibles et classes de base mixin (`nelson.mixin.SetGet`, `Heterogeneous`, `Copyable`, `CustomDisplay`).
- **Indexation personnalisée et tableaux d'objets** : construire des conteneurs ou des objets mandataires, indexer et concaténer des tableaux d'objets, et dériver des types numériques, logiques et caractères intégrés.
- **Introspection et persistance** : `metaclass` et `?ClassName` renvoient des métadonnées typées, et les objets personnalisés se sauvegardent et se rechargent sans perte.

### NFlow : éditeur visuel de schémas-blocs (1.0.0-beta.1)

- **Éditeur de schémas** : glisser et connecter des blocs ; les fils sont routés automatiquement. Les schémas sont enregistrés dans des fichiers texte, compatibles avec tout système de gestion de versions.
- **Bibliothèque de blocs** : sources, mathématiques, dynamiques continues et discrètes, logique, tables de correspondance, bus, magasins de données et puits. Tous les blocs acceptent des signaux vectoriels, matriciels et typés.
- **Modélisation physique acausale** : composants électriques, de translation, de rotation, thermiques et multicorps 2-D plans connectés par des broches physiques, résolus comme un système algébro-différentiel.
- **Sous-systèmes conditionnels et itérés** : sous-systèmes activés, déclenchés, réinitialisables, action, if/switch-case, appel de fonction, for-each et itérateurs For/While.
- **Simulation** : solveurs à pas fixe et adaptatif, moteur CVODES/IDAS optionnel pour les systèmes raides, localisation des passages par zéro, résolution des boucles algébriques et périodes d'échantillonnage multiples ; `sim()`, `linmod` et `trim` pilotent les modèles depuis des scripts.
- **Génération de code et FMI** : génération C ou Rust autonome depuis le même schéma, import FMI 2.0/3.0, export FMI 3.0 Co-Simulation et import/export SSP.
- **API de modèle scriptée** : `new_system`, `add_block`, `add_line`, `get_param`/`set_param`, `find_system` et fonctions associées ; les modifications faites par script apparaissent immédiatement dans l'éditeur.

### Bureau web et interface navigateur (aperçu)

- **Tout le bureau dans un navigateur** : fenêtre de commandes, explorateur d'espace de travail, historique, explorateur de fichiers, éditeur de variables, éditeur de code et figures dans des panneaux ancrables avec thèmes clair et sombre.
- **Deux modes d'exécution** : le lanceur `nelson-webview` ouvre par défaut une fenêtre webview native, ou sert le même bureau en HTTP avec `--web`.
- **Éditeur de code** : un éditeur fondé sur Monaco, avec autocomplétion, aide au survol, exécution de sections `%%`, débogage en direct et un panneau Problèmes avec corrections rapides.
- **Figures et interfaces sans Qt** : un moteur de rendu par liste d'affichage avec WebGL2 pour la 3-D, rotation/déplacement/zoom interactifs, `uicontrol` et composants ui rendus comme des widgets HTML, et boîtes de dialogue standard en modales.
- **Autres panneaux** : profileur, navigateur de documentation, terminal système, boîte d'import de données, galerie d'exemples et gestionnaire de paquets.
- **Version WebAssembly** : Nelson se compile aussi en WebAssembly. Le même bureau web s'exécute alors entièrement dans le navigateur, sans serveur ni installation, et le moteur peut être utilisé depuis Node.js. Cette version est monothread et exclut les extensions natives, la création de processus et la génération de code ; `iswasm` indique la plateforme.

### Nouveaux types de données

- **Dates et heures** : `datetime`, `duration` et `calendarDuration` avec arithmétique calendaire, fuseaux horaires et heure d'été, et valeurs manquantes `NaT`.
- **Tables, timetables et séries temporelles** : jointures de type SQL, pivots, résumés par groupe, gestion des données manquantes, `timetable` avec `retime` et synchronisation, `timeseries`, et calcul direct sur les données tabulaires avec propagation des unités.
- **Tableaux catégoriels** : un type `categorical` natif avec gestion complète des catégories, catégories ordinales, opérations ensemblistes et intégration aux tables.
- **Fonctions de texte** : `compose`, `extractBetween`, `insertAfter`, `pad`, `split`, `strjoin` et fonctions associées, plus `nelson.lang.makeValidName` et `makeUniqueStrings`.
- **Matrices creuses** : prise en charge `single` et simple complexe, solveurs itératifs (`gmres`, `pcg`, `bicgstab`, `lsqr` et autres) avec préconditionneurs, `ilu`/`ichol`, et `eigs`/`svds` pour les entrées creuses simple précision.

### Méthodes numériques

- **Solveurs ODE et DAE** : une interface commune `ode_solvers`, un moteur SUNDIALS optionnel, bascule automatique raide/non raide, équations différentielles à retard, analyse de sensibilité et jacobiennes creuses.
- **Optimisation** : programmation linéaire, linéaire mixte en nombres entiers, quadratique, non linéaire, moindres carrés et flux de travail par problème ; `fsolve`, `lsqnonlin` et `fmincon` implémentent les familles d'algorithmes standard et les structures de sortie.
- **Statistiques** : statistiques descriptives, ajustement de plus de quinze familles de distributions, corrélation et régression (linéaire, linéaire généralisée, robuste, ridge, lasso), tests d'hypothèses, partitionnement (`kmeans`, `kmedoids`, hiérarchique, spectral, mélanges gaussiens) et modèles de classification avec `predict`.
- **Traitement d'images** : fonctions 2-D et 3-D pour les conversions de couleurs, le filtrage, la détection de contours, la morphologie, les composantes connexes et mesures de régions, les transformations géométriques, le recalage, la segmentation et la détection de points d'intérêt. Les opérations lourdes sont parallélisées avec OpenMP.

### Graphiques et interfaces utilisateur

- **Nouveaux types de graphiques** : camemberts et anneaux, graphiques à bulles, tracés polaires, boîtes à moustaches, visualisation de volumes (`isosurface`), annotations de figures et éclairage de surfaces (`light`, `camlight`, `material`, `shading`, `fsurf`).
- **Utilitaires d'objets graphiques** : `gobjects`, `gco`, `gcbo`, `gcbf`, `allchild`, `findall`, `findfigs`, `copyobj`, `reset`, avec une couverture de propriétés bien plus large sur tous les objets graphiques.
- **Composants d'application pour `uifigure`** : panneaux, groupes de boutons, onglets, grilles, cases à cocher, boutons radio et bascules, listes déroulantes, zones de texte, compteurs, curseurs, molettes, jauges, voyants, interrupteurs, sélecteurs de date, `uitable`, `uitree` et `uiaxes`.
- **Rendu plus prévisible** : géométrie des figures alignée, éclairage dans l'espace caméra, rendu groupé des lignes pour les scènes 3-D denses et tracés natifs pour `patch`, `fill`, `fill3` et `area`.

### Interopérabilité et IA

- **Formats de fichiers** : lecture/écriture Open XML `.xlsx` sur toutes les plateformes, module `netcdf` optionnel, prise en charge optionnelle de Parquet et API de documents XML (`xmlread`, `xmlwrite`, `xslt`, `readstruct`, `writestruct`).
- **Python** : le paquet Python `nelson` pilote une session Nelson depuis Python, NumPy et pandas se convertissent dans les deux sens, et `pyfunction` permet au code Python de rappeler Nelson.
- **Intégration** : une nouvelle API C/C++ embarque un interpréteur Nelson complet dans d'autres applications.
- **Assistants IA** : un serveur MCP intégré permet aux assistants et agents IA d'interagir directement avec Nelson.

### Qualité de code, tests et paquets

- **Analyse statique** : `checkcode` et `codeIssues` détectent complexité, variables et imports inutilisés, masquage, code inaccessible et problèmes de style, avec diagnostics en ligne dans l'éditeur et corrections en un clic.
- **Outils en ligne de commande** : `nelson-lint` pour l'intégration continue (SARIF, JSON, texte brut) et `nelson-lsp`, un serveur de langage avec diagnostics en direct, formatage, aide au survol et corrections rapides.
- **Tests** : le lanceur `nelson.unittest` avec rapports HTML, JSON, JUnit XML et TAP13, le module `mocking` (`nelson.mock`) et une API d'assertions `asserts.*` par méthodes.
- **Gestionnaire de paquets** : `nmm` ajoute la recherche dans les registres, la résolution récursive des dépendances avec contraintes de versions sémantiques, des fichiers de verrouillage avec sommes SHA-256, des registres signés Ed25519, des installations transactionnelles avec retour arrière et des flux de publication.
- **Démarrage plus rapide** : les bibliothèques dynamiques sont chargées à la première utilisation, grâce à un cache de descripteurs de passerelles.

## Fonctionnalités principales

### Types de données

- **Double, simple et complexe** : scalaires, vecteurs, matrices 2-D, tableaux N-dimensionnels et matrices creuses.
- **Logiques et entiers** : types signés et non signés 8, 16, 32 et 64 bits.
- **Tableaux de caractères et de chaînes** avec prise en charge complète d'UNICODE.
- **Structures, cellules, dictionnaires, tables, timetables, tableaux catégoriels, dates et durées**.
- **Objets handle, classes `classdef` et fonctions anonymes** ; tous les types peuvent être surchargés.

### Performance

- **OpenMP et SIMD** pour le parallélisme et la vectorisation.
- **Calcul parallèle** sur processeurs multicœurs, **MPI** pour le calcul distribué et un module optionnel **`gpu_engine`** pour le calcul GPU via WebGPU.
- **FFT haute performance** fondée sur FFTW et MKL.

### Visualisation et interface

- **Tracés 2-D et 3-D** avec commandes de haut niveau et deux moteurs de rendu (Qt et web).
- **Contrôles d'interface utilisateur** et composants `uifigure` pour construire des applications personnalisées.
- **Environnement de bureau** avec historique des commandes, explorateur de fichiers, explorateur d'espace de travail, éditeur de variables, éditeur de code intégré et panneau terminal.

### Modules scientifiques

- Outils de **systèmes de contrôle** et interface **SLICOT** optionnelle.
- **Traitement du signal**, **polynômes**, **géométrie**, **fonctions spéciales**, calcul **symbolique**, **traitement d'images**, **statistiques**, **optimisation** et **solveurs ODE**.

### Formats de données et interfaces

- **JSON, XML, HDF5** (format d'espace de travail `.nh5` par défaut), **MAT-file**, **`.xlsx`**, **NetCDF** et **Parquet**.
- **Interface de fonctions étrangères (FFI)** : construction et chargement à la volée de code C/Fortran.
- Compatibilité **API MEX C** et **API Nelson Engine**.
- Interfaces **Julia** et **Python**, **API RESTful**, **communication inter-processus**, **moteur QML** et composants **COM** sous Windows.

### Aide, tests et profilage

- **Moteur d'aide** générant la documentation en HTML, Markdown, PDF ou GitBook.
- **Moteur de test** avec export de rapports xUnit, JUnit, TAP13 et HTML.
- Outils de **profilage** et de **couverture de code**.

## Ressources

- **Site web** : [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/)
- **Code source et tickets** : [https://github.com/nelson-lang/nelson](https://github.com/nelson-lang/nelson)
- **Nelson Cloud** : utiliser Nelson depuis un navigateur web avec [Nelson Cloud](https://www.npmjs.com/package/nelson-cloud).
- **Nelson Modules Manager (nmm)** : installer et gérer les extensions de Nelson.
- **Squelettes de module** pour étendre Nelson :
  - [Template avec Macros et Builtins](https://github.com/nelson-lang/module_skeleton).
  - [Template de Macros de Base](https://github.com/nelson-lang/module_skeleton_basic).

---

## Journal des modifications

- [Journal des modifications v2.x.x](CHANGELOG.md)
- [Journal des modifications v1.x.x](CHANGELOG-1.x.x.md)
- [Journal des modifications v0.7.x](CHANGELOG-0.7.x.md)
- [Journal des modifications v0.6.x](CHANGELOG-0.6.x.md)
- [Journal des modifications v0.5.x](CHANGELOG-0.5.x.md)
- [Journal des modifications v0.4.x](CHANGELOG-0.4.x.md)
- [Journal des modifications v0.3.x](CHANGELOG-0.3.x.md)
- [Journal des modifications v0.2.x](CHANGELOG-0.2.x.md)
- [Journal des modifications v0.1.x](CHANGELOG-0.1.x.md)

---

## Licence

- [Licence de Nelson](license.md)
