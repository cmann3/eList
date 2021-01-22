# eList 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed bug involving nested comprehensions.
* Updated `flatten` methods so that its properly works with lists.
* Updated `Num`, `Chr`, `Logical`, and `Vec` to flatten results before conversion so that repeated `NA` results are avoided.


# eList 0.2.0

* Improved support for braces within nested comprehensions. Comprehension functions now recognize `{for ...}` as a vectorized operation.
* Improved support for `<-` assignment within comprehensions. Comprehension functions now recognize `name <- for ...` as a vectorized operation. 
* Fixed environment handling so that objects are searched within the Global environment before the base. 
* Added support for commas when using multiple variables.
* Removed functional programming functions: `reduce`, `filter`, `map`, etc. These will be placed in the `eFun` package.
