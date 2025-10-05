<link rel="stylesheet" href="highlight.css"/>
<script src="highlight.pack.js"></script>
<script>hljs.highlightAll();</script>

# Démarrage avec Nelson

Bienvenue dans Nelson : un langage de calcul numérique de haut niveau inspiré de MATLAB(c)/Octave et conçu pour un travail d'ingénierie et scientifique rapide et productif.

Ce guide vous propose un chemin rapide de l'installation à l'exécution de vos premières commandes, l'écriture de scripts, le tracé de graphiques et la recherche d'aide. Il est destiné aux nouveaux utilisateurs qui souhaitent un démarrage rapide et quelques exemples prêts à l'emploi.

Ce tutoriel est organisé en courtes leçons que vous pouvez suivre dans le REPL ou en exécutant de petits fichiers de script.

---

## Leçons du tutoriel

### Introduction

Nelson est un langage de calcul numérique de haut niveau dont le type de données central est le tableau (vecteurs et matrices). Comme MATLAB, Nelson met l'accent sur l'informatique interactive, la visualisation et le prototypage rapide. Ce tutoriel vous aide à apprendre l'essentiel : utilisation du REPL, opérations mathématiques de base, tracés, tableaux, scripts et constructions de programmation simples.

### Fonctionnalités principales

- REPL interactif avec historique et aide
- Tableaux natifs (vecteurs, matrices, tableaux N‑D) sans dimensionnement explicite
- Fonctions mathématiques et matricielles intégrées (algèbre linéaire, fft, statistiques)
- API de tracé de haut niveau pour des graphiques rapides
- Modules et gestionnaire de modules extensible (`nmm`)
- Interface de fonction étrangère (FFI), interop Python/Julia et I/O d'espace de travail (`.nh5`, `.mat`)

### Une session minimale Nelson

Cette session rapide montre comment démarrer, effectuer des calculs simples et quitter.

#### Démarrer Nelson

Ouvrez un terminal (PowerShell sous Windows, bash/zsh sous Unix) et lancez :

```bash
nelson
```

Vous verrez une invite. Cette invite accepte des expressions Nelson.

#### Utiliser Nelson comme calculatrice

À l'invite, essayez :

```matlab
a = 1 + 2 * 3
b = sin(pi/4)
```

Si vous n'affectez pas de variable, les résultats sont stockés dans la variable par défaut `ans`.

#### Quitter Nelson

Tapez :

```matlab
quit
```

ou utilisez le raccourci du REPL pour quitter.

### Démarrage : variables et espace de travail

#### Créer des variables

Les variables sont créées par affectation :

```matlab
x = 2*pi
v = [1, 2, 3]
```

Nelson utilise des crochets pour les vecteurs et des points-virgules pour séparer les lignes dans les matrices :

```matlab
A = [1 2; 3 4]
```

#### Réaffectation et suppression de l'affichage

La réaffectation est autorisée. Pour empêcher l'affichage immédiat, terminez une instruction par `;` :

```matlab
t = 5;
```

#### Messages d'erreur et corrections

Les fautes de frappe produisent une erreur — utilisez la flèche haut pour rappeler les commandes précédentes, éditez-les et relancez.

#### Priorité des opérateurs

Utilisez des parenthèses pour contrôler la priorité :

```matlab
(1+2)*3    % donne 9
1+2*3      % donne 7
```

#### Gérer l'espace de travail

Commandes utiles :

- `clear` — supprimer toutes les variables
- `who` — lister les noms de variables
- `whos` — liste détaillée (taille, type)
- `save('s.nh5')` — sauvegarder l'espace de travail (HDF5 `.nh5` par défaut)
- `load('s.nh5')` — restaurer l'espace de travail

#### Tenir un journal de session

Enregistrez toutes les entrées/sorties avec :

```matlab
diary('session.txt')
diary off
```

---

## Fonctions mathématiques et tracés

### Fonctions mathématiques élémentaires

Nelson fournit un riche ensemble de fonctions mathématiques : `sin`, `cos`, `tan`, `exp`, `log`, `sqrt`, `abs`, et plus encore. Des constantes telles que `pi`, `Inf` et `NaN` sont disponibles.

Exemples :

```matlab
y = exp(-5)*sin(2) + 10*sqrt(8)
log(142)
sin(pi/4)
```

### Tracé de base

Pour tracer des données, préparez des vecteurs `x` et `y` puis appelez `plot` :

```matlab
x = linspace(0, 2*pi, 201)
y = sin(x)
plot(x, y)
xlabel('x')
ylabel('sin(x)')
title('Sine')
```

Jeux de données multiples :

```matlab
plot(x, 2*cos(x), '--', x, cos(x), '-', x, 0.5*cos(x), ':')
legend('2*cos(x)', 'cos(x)', '0.5*cos(x)')
```

Les styles de lignes, couleurs et marqueurs suivent des codes courts courants : `r`, `b`, `k`, `--`, `:`, `o`, `*`, etc.

---

## Tableaux, matrices et équations linéaires

### Créer des vecteurs et des matrices

Vecteur ligne :

```matlab
v = [1 4 7 10]
```

Vecteur colonne :

```matlab
w = [1; 4; 7; 10]
```

Transposé :

```matlab
w = v'
```

Indexation et sous-matrices :

```matlab
v(1:3)      % éléments 1 à 3
A(2,:)      % deuxième ligne
A(:,2:3)    % colonnes 2 et 3
```

Opérateur deux-points et `linspace` :

```matlab
0:0.1:5
linspace(0, 2*pi, 101)
```

### Opérations élément-par-élément vs matricielles

- Multiplication matricielle : `A * B`
- Multiplication élément-par-élément : `A .* B`

Utilisez le préfixe `.` pour les opérateurs élément-par-élément : `.*`, `./`, `.^` quand nécessaire.

### Résoudre des systèmes linéaires

Résoudre `Ax = b` avec l'opérateur antislash (préféré pour la stabilité numérique) :

```matlab
x = A \ b
```

Vous pouvez aussi calculer `inv(A)*b` mais l'utilisation de `A\b` est généralement recommandée.

---

## Introduction à la programmation dans Nelson (scripts & fonctions)

### Scripts

Créez des fichiers de script (par exemple `example1.m`) contenant une séquence de commandes. Exécutez-les avec :

```bash
nelson -f example1.m
```

Exemple de script simple (enregistrer sous `example1.m`) :

```matlab
% example1.m
A = [1 2 3; 3 3 4; 2 3 3];
b = [1; 1; 2];
x = A \ b
```

Les variables créées dans un script sont placées dans l'espace de travail global (Attention aux possibles effets de bord).

### Fonctions

Les fonctions possèdent leur propre espace de travail local et évitent de polluer l'espace global. Exemple de fichier de fonction `fact.m` :

```matlab
function f = factorial(n)
    % FACTORIAL(n) Calculer la factorielle en utilisant prod
    f = prod(1:n);
end
```

Appelez avec `factorial(5)` pour obtenir `120`.

### Entrée et sortie

Interrogez l'utilisateur dans un script avec `input(...)` et formatez la sortie avec `printf`/`disp` ou équivalents disponibles dans Nelson.

---

## Contrôle de flux et opérateurs

### If / for / while

If structure :

```matlab
if expr
    statements
elseif expr2
    statements
else
    statements
end
```

Boucle for :

```matlab
for i = 1:5
    s = i*i
end
```

Boucle while :

```matlab
while x <= 10
    x = 3*x
end
```

### Opérateurs relationnels et logiques

Comparaisons : `>`, `<`, `>=`, `<=`, `==`, `~=`  
Logiques : `&`, `|`, `~` (élément-par-élément) et `&&`, `||` (court-circuit là où supporté)

---

## Annexe : Récapitulatif des commandes utiles

Cette liste courte rassemble les commandes les plus fréquemment utilisées.

- Général

  - `quit` — quitter Nelson
  - `doc <command>` — afficher l'aide pour une commande

- Espace de travail et fichiers

  - `clear`, `who`, `whos`, `save('file.nh5')`, `load('file.nh5')`, `diary('session.txt')`

- Tableaux et matrices

  - opérateur `:`, `linspace(a,b,n)`, `zeros(m,n)`, `ones(m,n)`, `eye(n)`

- Algèbre linéaire

  - `A\b` résoudre, `inv(A)`, `det(A)`, `eig(A)`, `rank(A)`

- Tracés
  - `plot(x,y)`, `xlabel()`, `ylabel()`, `legend()`, `title()`, `axis()`

---

Bon calcul avec Nelson !
