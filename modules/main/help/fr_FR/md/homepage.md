![banner](banner_homepage.png)

### Nelson 1.10.0.0

**Nelson** est un langage de calcul numérique puissant et open-source, conçu pour offrir un environnement complet et intuitif aux ingénieurs, scientifiques et étudiants. Avec plus de 1 200 fonctions intégrées, Nelson prend en charge une large gamme de tâches, allant de l'algèbre de base aux simulations numériques avancées.

Initialement inspiré par des langages comme MATLAB© et Gnu Octave, Nelson offre aux utilisateurs une expérience légère mais riche en fonctionnalités. Il est conçu pour être facile à apprendre et à utiliser, avec un accent particulier sur la performance et la flexibilité.

**Essayez-le !**

[Site Web](https://nelson-lang.github.io/nelson-website/)

## Fonctionnalités

### Types de données gérés par Nelson

- **Double et Complexe Double** : Prend en charge les scalaires, les vecteurs, les matrices 2D, les tableaux N-dimensionnels et les matrices creuses.
- **Simple et Complexe Simple** : Inclut les scalaires, les vecteurs, les matrices 2D, les tableaux N-dimensionnels et les matrices creuses.
- **Logique** : Gère les scalaires, les vecteurs, les matrices 2D, les tableaux N-dimensionnels et les matrices creuses.
- **Tableaux de caractères** : Prend en charge les caractères UNICODE.
- **Tableaux de chaînes** : Prend en charge pleinement UNICODE.
- **Entiers** : Scalaires signés et non signés 8, 16, 32 et 64 bits, vecteurs, matrices 2D et tableaux N-dimensionnels.
- **Objets Handle** : Pour la fonctionnalité orientée objet.
- **Fonctions anonymes** : Permet la création et la manipulation de fonctions sans nom.
- **Structures de données** : Prend en charge les dictionnaires et les tableaux.
- **Surcharge** : Tous les types peuvent être surchargés pour un comportement personnalisé.

### Améliorations de la performance

- **OpenMP et SIMD** : Utilise le traitement parallèle et la vectorisation pour des calculs plus rapides.

### Visualisation & Interface

- **Graphiques 2D et 3D** : Commandes de haut niveau pour la visualisation.
- **Contrôles d'interface utilisateur** : Contrôles intégrés pour créer des interfaces personnalisées.
- **Environnement de bureau** : Suivi de l'historique, explorateur de fichiers et navigateur de l'espace de travail.

### Modules avancés

- **Calcul parallèle** : Permet une utilisation efficace des processeurs multi-cœurs.
- **Transformée de Fourier rapide (FFT)** : Fonctions FFT haute performance basées sur FFTW et MKL.
- **Interface SLICOT** : Support optionnel pour la bibliothèque de sous-programmes en théorie des systèmes et du contrôle.
- **Module Système de contrôle** : Outils pour la théorie du contrôle et la conception de systèmes.
- **MPI (Interface de passage de messages)** : Fonctions pour le calcul parallèle distribué.

### Formats de données & Interface

- **Support JSON** : Lecture et écriture de fichiers JSON.
- **Fonctions HDF5** : Fonctions I/O de haut niveau, avec HDF5 comme format de fichier par défaut pour les espaces de travail `.nh5`.
- **Compatibilité MAT-File** : Chargement et sauvegarde des espaces de travail au format MAT-file.
- **Interface de fonction étrangère (FFI)** : Construction et chargement dynamique de code C/Fortran.
- **Compatibilité API MEX C** : Interface avec les API C compatibles MEX.
- **API Nelson Engine** : Utilisation de Nelson comme moteur en arrière-plan dans le code C, compatible avec l'API MEX Engine.
- **Interface Python** : Appel de scripts et fonctions Python depuis Nelson.
- **API RESTful** : Permet à Nelson d'interagir avec des services web.

### Fonctionnalités supplémentaires

- **Communication inter-processus** : Communication entre les processus Nelson.
- **Moteur QML** : Utilisation du framework QML de Qt pour afficher et manipuler du contenu graphique.
- **Component Object Model (COM)** : Interface avec les composants COM, notamment sous Windows.
- **Support des fichiers Excel** : Lecture et écriture de fichiers `.xlsx` via COM sous Windows.
- **Éditeur de code intégré** : Éditeur intégré pour les scripts Nelson.

### Outils d'aide et de test

- **Moteur d'aide** : Génération et visualisation des fichiers d'aide dans divers formats comme HTML, Markdown, PDF ou GitBook.
- **Moteur de test** : Validation des algorithmes avec des fonctions intégrées, avec export des rapports xUnit.

### Profilage & Couverture de code

- **Profileur** : Profileur intégré pour analyser et optimiser la performance du code.
- **Couverture de code** : Mesure de la couverture des tests pour garantir une validation complète.

### Cloud & Extensibilité

- **Nelson Cloud** : Accès instantané à Nelson depuis n'importe quel navigateur web via [Nelson Cloud](https://www.npmjs.com/package/nelson-cloud).
- **Module Skeleton** : Modèles pour étendre Nelson :
  - [Template avec Macros et Builtins](https://github.com/nelson-lang/module_skeleton).
  - [Template de Macros de Base](https://github.com/nelson-lang/module_skeleton_basic).
- **Nelson Modules Manager (nmm)** : Un gestionnaire de paquets pour installer et gérer les extensions de Nelson.

---

- [Journal des modifications](CHANGELOG.md)
- [Journal des modifications v0.7.x](CHANGELOG-0.7.x.md)
- [Journal des modifications v0.6.x](CHANGELOG-0.6.x.md)
- [Journal des modifications v0.5.x](CHANGELOG-0.5.x.md)
- [Journal des modifications v0.4.x](CHANGELOG-0.4.x.md)
- [Journal des modifications v0.3.x](CHANGELOG-0.3.x.md)
- [Journal des modifications v0.2.x](CHANGELOG-0.2.x.md)
- [Journal des modifications v0.1.x](CHANGELOG-0.1.x.md)
- [Licence de Nelson](license.md)
