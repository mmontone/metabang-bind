# metabang-bind

bind is let and much much more

## What it is

Bind extends the idea of of `let` and destructing to provide a uniform syntax for all your accessor needs. It combines _let_, _destructuring-bind_, `with-slots`, `with-accessors`, structure editing, property or association-lists, and _multiple-value-bind_ and a whole lot more into a single form. The [user guide](https://common-lisp.net/project/metabang-bind/user-guide.html) has all the details but here is example to whet your appetite:

```lisp
    (bind ((a 2)  
           ((b &rest args &key (c 2) &allow-other-keys) '(:a :c 5 :d 10 :e 54))  
           ((:values d e) (truncate 4.5)))  
      (list a b c d e args))  
    ==> (2 :A 5 4 0.5 (:C 5 :D 10 :E 54)) 
```

Bind is especially handy when you have more than one layer of `multiple-value-bind` or `destructuring-bind`. Since `bind` is a single form, you don't end up too far off to the right in editor land.

Bind is released under the [MIT license](http://www.opensource.org/licenses/mit-license.php)

## What is happening

- 10 April 2010 - moved to github; added flet support

- 28 May 2009 - added `:structure/rw` binding form; updated webpage to link to the user's guide

- 1 Dec 2007 - Added support for [array destructuring](user-guide.html#array-bindings) (Thanks to Tamas Papp for the idea)

- 15 Nov 2007 - New user guide; bind handles structures and property lists and is now extensible!

- 13 Nov 2005 - Initial webpage n' stuff.

## This fork

This is a fork of metabang-bind.

At the moment, it adds:

- Tests.
- Code indentation fixes.
- Fixes to regex binding form.
- &rest arguments in plists binding form.
- hash-table binding forms.
