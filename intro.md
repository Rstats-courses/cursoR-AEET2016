R Intro
========================================================
author: I. Bartomeus
date: Nov 2016
autosize: true

Como serán los siguientes 5 días de vuestra vida?
=========================================================

* Introducción (yo hablando; 45')
* Programación en R (yo) + Visualización de datos (Paco)
* Modelos mixtos (yo) + Modelos nulos (yo)
* Bayesiano (Paco) + Simulaciones (yo)
* Multivariante (yo) + git (Paco)
* GIS (Paco) + "a jugar" (Paco y yo)

Feedback: <a href="nacho.bartomeus@gmail.com">Email: nacho.bartomeus@gmail.com</a>  

[Twitter @ibartomeus](https://twitter.com/ibartomeus)


Como se estructuran las clases
=======================================================

- Cursos de R online y libros hay a patadas. 
- Aquí venimos a equivocarnos cuanto más mejor
- Resolveremos cuantos más problemas mejor
- Sistema de posit it para los ejercicios.

Objetivos
=======================================================

- Entender las ventajas de usar R (o otros lenguages de programación)
- Que descubrais que con R se puede hacer casi todo (desde esta presentación hasta pedir pizza)
- Saber suficiente R para poder "googlear" lo que necesiteis aprender/resolver a partir de ahora.

Los Básicos
=======================================================

- Download R
- R desde Rstudio (download Rstudio)
- [Material del curso](https://github.com/Rstats-courses/cursoR-AEET2016)
- Carpeta 'ejercicios' y 'ejercicios_resueltos'


Y si no se algo? Uso de Stackoverflow.
========================================================

- [StackOverflow](http://stackoverflow.com)
- [How do I ask a good question?](http://stackoverflow.com/help/how-to-ask)
- Google (e.g. error message + r)

Baremo del problema:
- consulta (hasta 5 pestañas abiertas)
- problema (hasta 10 pestañas abiertas)
- marrón (> 10 pestañas)


Por que R?
========================================================

>R has simple and obvious appeal. Through R, you can sift through complex data sets, manipulate data through sophisticated modeling functions, and create sleek graphics to represent the numbers, in just a few lines of code...R’s greatest asset is the vibrant ecosystem has developed around it: The R community is constantly adding new packages and features to its already rich function sets.
>
>-- [The 9 Best Languages For Crunching Data](http://www.fastcompany.com/3030716/the-9-best-languages-for-crunching-data)


Seguro que R es la herramienta adecuada?
========================================================

No siempre. R tiene limitaciones y debilidades:
- Curva de aprendizage; syntaxis incosistente
- Documentación fragmentada (?help, vignettes, etc...)
- Calidad de los paquetes varia
- No esta diseñado para grandes bases de datos (~100 Mb de csv)

Hay otras herramientas:
- Julia, Python, C++, bash, ...
- Excel? Casi nunca.


La verdadera ventaja de usar R: Reproducibilidad
========================================================

> It’s important to make a workflow that you can use time and time again, and even pass on to others in such a way that you don’t have to be there to walk them through it. [Source](http://berkeleysciencereview.com/reproducible-collaborative-data-science/)

> Your closest collaborator is you 6 months ago, and you don't respond to emails.
<small>P. Wilson</small>

Interested: [read our paper](http://www.revistaecosistemas.net/index.php/ecosistemas/article/view/1178)

No puedes reproducir
========================================================
...Lo que no existe.
- Gozilla se ha comido mi ordenador
  + backup
  + idealmente de forma continua
- Godzilla se ha comido mi oficina
  + cloud



No puedes reproducir
========================================================

...lo que has perdido. Y si necesitas un archivo que existio hace 1, 10 o 100 dias?
- Incremental backups (minimo)
- Version control (mejor). **Git** (y **GitHub**) es el más popular


Estadística
=============================================

- Es una ciencia dinámica.
- Es una ciencia compleja y con diversas filosofias.
- Es una herramienta, no un fin.
- No puede solucionar malos datos (e.g. baja replicación). 
- No puede solucionar un mal diseño experimental.


Abrir R studio
=============================================

- scripts
- consola
- environment
- files/plots


Trabajando con proyectos.
========================================================

Directorio tipico:
```
1-get_data.R
2-process_data.R
3-analyze_data.R
4-make_graphs.R
data/
figures/
```


Guias de estilo
==============================================

>Da igual cual sigas, lo importante es tener uno <small>I. Bartomeus</small>

El mio es [este](https://github.com/ibartomeus/misc_func/blob/master/Style.md)
El de google [este](https://google.github.io/styleguide/Rguide.xml)



Resources
========================================================
  
* [CRAN](http://cran.r-project.org) - The Comprehensive R Archive Network. Ground zero for R.
* [GitHub](https://github.com/) - The GitHub page.
* [RStudio](http://www.rstudio.com) - the integrated development environment for R. Makes many things easier.
* [Advanced R](http://adv-r.had.co.nz) - the companion website for “Advanced R”, a book in Chapman & Hall’s R Series. Detailed, in depth look at many of the issues covered here.


Resources
========================================================
  
- [CRAN task views](https://cran.r-project.org/web/views/) resumenes de tareas (e.g. Bayesiano)
- e.g. [**Reproducible Research**](http://cran.r-project.org/web/views/ReproducibleResearch.html)
- [RopenScience](https://ropensci.org/). Paquetes brutales para hacer ciencia abierta


Resources
========================================================

- [Quick-R](http://www.statmethods.net/): Donde yo voy ha consultar las recetas (PCA's, glm's, etc...)
- Cursos para empezar:
  *[r for cats](http://rforcats.net/)
  *[r-bio](http://r-bio.github.io/01-intro-R/)
  *[datacamp](https://www.datacamp.com/courses/free-introduction-to-r?utm_source=blog_launch_new_intro&utm_medium=blog&utm_campaign=launch_new_intro)
  

Más?
===============================================

y el [grupo de usuarios de R de Sevilla](https://sevillarusers.wordpress.com)





