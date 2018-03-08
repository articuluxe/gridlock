# gridlock #

Traverse and explicate grid structures using emacs.

## Why? ##

Just. Because.

## No, really. Why? ##

  * Ever been lost in a large .csv file and wished you knew what heading your cursor was on?
  * Ever been trolling around a fix log file and wondered what tag *that* was?
  * Amaze onlookers with your effortless aplomb.
  * Inspire envy from other would-be studiers of structured data pathetically frustrated by inscrutable formats.

`csv-mode` is nice and all, and `csv-align-fields` is a perfectly cromulent method to align columns.  But it doesn't help when the length or number of fields exceeds the size of your window.  Then you are, if you will permit me to be technical, SOL.  Let `gridlock-mode` guide you through the fog.  With it, you are able to:  

  *  move between fields on the same line (forward and backward, jumping to start and end or a particular field)  
  *  jump to the next or previous line  
  *  all the while showing a helpful heading detailing where you are.  
  
Not since sliced bread, I tell ya.  

### Display Formats ###

There are different methods built-in to gridlock to help display where you are.  It comes with:

  * [popup](https://github.com/auto-complete/popup-el)
  * [pos-tip](https://github.com/pitkali/pos-tip)
  * [quick-peek](https://github.com/cpitclaudel/quick-peek)
  * minibuffer echo

You are able to interactively select your desired display scheme.  The set of available schemes is automatically updated based on whatever libraries you have installed.  Worry not, `gridlock` also works in a terminal, though note that `pos-tip` requires `(display-graphic-p)` to be true.

## Demonstration ##

You'd like that, wouldn't you.  


Oh, OK, here you go:

## Installation ##

Ensure the gridlock .el files are in your load-path.  Then craft something like the following `use-package` stanza:

`(use-package gridlock-csv  
  :after csv-mode  
  :bind (:map csv-mode-map ("C-c C-l" . gridlock-csv-mode)))
`  

Then literally, just, like, open a csv file, man.  

### Flexibility ###

`gridlock-mode` is also intended to be used in file formats other than csv.  Anything that has a structure than can be described by regular expressions can be attuned to the splendor of gridlock's capabilities.  Here, authors of unforetold minor-modes of data exploration, are the variables with which gridlock's capabilities can be adjusted:

#### gridlock-anchor-regex ####

This regex identifies lines of interest.  Lines without a match will be ignored.  Is every line interesting, like in a typical csv file?  Then set this to its default, "^".  Otherwise, this regex identifies what gridlock considers the anchor point:  the point at which further field processing begins.

#### gridlock-field-delimiter ####

Once you've identified lines of interest, you need to split them up into any fields that may be present.  This regex describes the delimiter that splits fields.  For the typical csv format, the default value is ",".

#### gridlock-field-regex-begin ####

Jewels may lie among cruft.  Even on lines of interest, there may be text before or after the fields: preambles or such.  This regex, if non-empty, describes the point on the line at which the first field begins.  If this regex is left empty, its default, then the fields are presumed to begin immediately after the anchor point.

#### gridlock-field-regex-end ####

Wheat shines among chaff.  This regex identifies the ending of the fields on lines of interest.  If left empty, its default, the field processing extends to the end of the line.

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




