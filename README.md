# hdis-as

Course materials for the audition certificates analysis

...


## To render book, run the following command:

`bookdown::render_book('index.Rmd', "bookdown::gitbook")`

### Settings:

```
bookdown::gitbook:
  css: style.css
  toc_depth: 1
```

`toc_depth` can be added to modify the way TOC appears.


### Time-consuming operations

The following should help with time-consuming operations and bookdown regenerations:

- run time-consuming operations while working on the code; save their results into RDS files;
- write the code that does this operations, but do not execute it during book compilation (unless it is really necessary);
- instead, during book generation, load pregenerated results (but hide this loading from the final look of the book); 


### Settings

- `split_by: chapter` in `_output.yml`: change the value to specify splitting: `chapter` keeps it essentially in the same way as you have it---convenient when sections are short; `section` will break it down into sections---convenient when sections are long; `none` will keep them all together;
