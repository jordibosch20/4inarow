# 4 en Ratlla

En aquesta pràctica de LP, realitzarem un programa amb Haskell que jugui interactivament contra el 
participant al 4 en ratlla

## Anar començant

Les seguents instruccions et diran com executar el programa.

### Prerequisits

Només necessites el compilador ghc per compilar codi Haskell. També necessites la llibreria System.Random

En cas que no tingueu instalat el ghc:

```bash
> sudo apt install ghc
```
I per instal·lar la llibreria Random

```bash
> sudo apt install cabal-install
> cabal update
> cabal install random
```

### Compilar

Per compilar només cal fer a la línia de comandes

```
ghc joc.hs
```

```
./joc
```

## Com jugar

A partir d'ara, seleccionem el nombre de files i columnes que volem i després el nivell contra el que volem jugar


### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Desenvolupament

Aquí mostraré alguns insights d'algunes funcions clau per entendre el funcionament.

El nivell 1, l'ordinador simplement selecciona una columna de manera Random.

El nivell 2, l'ordinador segueix una estratègia greedy. Això significa que sempre busca maximitzar el número
de fitxes consequtives que podria colocar i decideix la columna en base a aquest raonament.

Ens assegurem però, de que si el Participant pot fer-ne 4 seguides en el pròxim torn, intentem evitar-ho, sense
tenir en compte maximitzar les nostres fitxes.

Aquesta estrategia presenta certes mancances ja que no es capaç de preveure moviments pròxims del contrari que
el portaran a perdre, ja que molts cops actuar "greedilment" ens porta a mínims locals i no globals. Un clar
exemple es que la simple sequencia del Participant 3-4-2 guanya.

En el nivell 3, el que fem es és intentar anar més profundament en les possibles combinacions que el participant
pot fer i actuar en conseqüència. En el meu cas he decidit seguir una estratègia min-max. Això significa que
per decidir la columna on posaré la fitxa el que faig és simular que passaria en 2 nivells de profunditat si 
poses la fitxa en cada columna. És a dir, simulo que l'Ordinador posa la fitxa a cada columna, a partir d'aquí
miro cada possibilitat que pot posar el Participant, en aquest nou Tauler miro on posaria la fitxa l'Ordinador
(actuant greedilment) i a partir d'aquí miro totes les possibilitats que pot fer el Participant.
Llavors el que faig és escollir la columna que em dongui el mínim màxim, és a dir


## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc

