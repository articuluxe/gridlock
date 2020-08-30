# gridlock #

Traverse and explicate grid structures using emacs.

## Why? ##

Just. Because.

## No, really. Why? ##

  * Ever found yourself lost and adrift in a sprawling .csv file?  Would it have helped if a kind stranger had drawn you a map of where you were?
  * Ever been trolling around a FIX log file and wondered what tag *that* was?
  * Amaze onlookers with your effortless aplomb.
  * Inspire envy from other would-be studiers of structured data pathetically frustrated by inscrutable formats.

Stop depending on the niceness of strangers.  Empower yourself with `gridlock-mode`.  `csv-mode` is nice and all, and `csv-align-fields` is a perfectly cromulent method of aligning columns.  But it doesn't help when the length or number of fields exceeds the size of your window.  Then you are, if you will permit me to be technical, SOL.  Let `gridlock-mode` guide you through the fog.  With it, you are able to:  

  *  move between fields on the same line (forward and backward, jumping to start and end of line, or straight to a particular field, by content or heading)  
  *  jump to the next or previous line  
  *  all the while showing a helpful heading detailing where you are.  
  
Not since sliced bread, I tell ya.  

### Display Formats ###

There are different methods built-in to gridlock to help display where you are.  It comes with:

  * [popup](https://github.com/auto-complete/popup-el)
  * [pos-tip](https://github.com/pitkali/pos-tip)
  * [quick-peek](https://github.com/cpitclaudel/quick-peek)
  * minibuffer echo

You are able to interactively select your desired display scheme.  The set of available schemes is automatically updated based on whatever libraries you have installed.  Worry not, `gridlock` also works in a terminal, through regrettably one of the display schemes, `pos-tip`, is not available there as it requires `(display-graphic-p)` to be true.

## Demonstration ##

You'd like that, wouldn't you.  


Oh, OK, here you go:  


## Flavors ###

Gridlock is also intended to be used in file formats other than csv.  Anything that has a structure than can be described by regular expressions can be attuned to the splendor of Gridlock's capabilities.  Gridlock comes in two flavors right out of the box:

  * Gridlock-csv  
    `gridlock-csv-mode` is your CSV SAK (Swiss Army Knife).  
      
  Tag metadata comes from the first line.  That is, the headings shown when you alight on a column are sourced by the first line in the file, which is often a list of headings, possibly preceded by a comment character `#`.  
        
  * Gridlock-fix  
    `gridlock-fix-mode` can help you trawl through log files of your favorite FIX application.  Currently it knows about FIX 4.2 tags.  

## Installation ##

Ensure the gridlock .el files are in your load-path.  Then craft something like the following `use-package` stanzas:

#### CSV ####

```(use-package gridlock-csv  
  :after csv-mode  
  :bind (:map csv-mode-map ("C-c C-l" . gridlock-csv-mode)))
```  

Then literally, just, like, open a csv file, man.  

#### FIX ####

```(use-package gridlock-fix :bind ("C-c M-f" . gridlock-fix-mode))```

Note that `gridlock-fix-mode` also requires "fix4.2.hash" be in `load-path`.

### Details ###

Here, authors of unforetold minor-modes of data exploration, are the variables with which gridlock's capabilities can be unleashed:

#### gridlock-anchor-regex ####

This regex identifies lines of interest.  Lines without a match will be ignored.  Is every line interesting, like in a typical csv file?  Then set this to its default, "^".  Otherwise, if this regex is missing on a line, gridlock ignores that line.  In addition, if `gridlock-field-regex-begin` is empty (the default), the anchor point is treated as the point at which further field processing begins.

#### gridlock-field-delimiter ####

Once you've identified lines of interest, you need to split them up into any fields that may be present.  This regex describes the delimiter that splits fields.  For the typical csv format, the default value is ",".

#### gridlock-field-regex-begin ####

Jewels may lie among cruft.  Even on lines of interest, there may be text before or after the fields: preambles or such.  This regex, if non-empty, describes the point on the line at which the first field begins.  If this regex is left empty, its default, then the fields are presumed to begin immediately after the anchor point.

##### Technical Note #####
If this regex includes a capture group, then that capture group is presumed to be *important* and will be included in field parsing, that is, field parsing will begin at the beginning of the match.  This can be helpful if it's more convenient to demarcate the beginning of field matching by the first field itself, rather than a preamble that precedes it.  With no capture group, the regex is presumed to be *unimportant* and field parsing will begin at the end of the match.

#### gridlock-field-regex-end ####

Wheat shines among chaff.  This regex identifies the ending of the fields on lines of interest.  If left empty, its default, the field processing extends to the end of the line.

##### Technical Note #####
If this regex includes a capture group, then that capture group is presumed to be *important* and will be included in field parsing, that is, field parsing will continue up to the end of the match.  This can be helpful if it's more convenient to demarcate the end of field matching by the last field itself, rather than an epilog that follows it.  With no capture group, the regex is presumed to be *unimportant* and field parsing will continue up to the beginning of the match.

## Commands ##

#### gridlock-goto-next-line ####

###### Bound to the down arrow key and "n" by default. ######

Moves the cursor to the next valid line.  Attempts to maintain the current column.

#### gridlock-goto-prev-line ####

###### Bound to the up arrow and "p" by default. ######

Moves the cursor to the previous valid line.  Attempts to maintain the current column.

#### gridlock-goto-next-field ####

###### Bound to the right arrow and "f" by default. ######

Moves the cursor to the next field on the current line.

#### gridlock-goto-previous-field ####

###### Bound to the left arrow and "b" by default. ######

Moves the cursor to the prior field on the current line.  

#### gridlock-goto-line-start ####

###### Bound to "a" by default. ######

Moves the cursor to the first field on the current line.

#### gridlock-goto-line-end ####

###### Bound to "e" by default. ######

Moves the cursor to the last field on the current line.

#### gridlock-show-title ####

###### Bound to SPC by default.  ######

Shows the current grid title, in case it is not currently displayed.  
(Note that some display schemes, like `pos-tip`, may have a timeout.

#### gridlock-choose-display-scheme ####

###### Bound to "`" by default. ######

Interactively choose the display scheme in use.  It is updated immediately.

#### gridlock-jump-to-field-by-index ####

###### Bound to "v" by default. ######

Interactively choose a field to jump to, by index.

#### gridlock-jump-to-field-by-heading ####

###### Bound to "j" by default. ######

Interactively choose a field to jump to, by heading/title.

#### gridlock-jump-to-field-by-content ####

###### Bound to "g" by default. ######

Interactively choose a field to jump to, by content.

## CI ##
![CI](https://github.com/articuluxe/gridlock/workflows/CI/badge.svg)


