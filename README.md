# GitChapter

## What problem does this solve?
Allows you to embed source code, shell output, and various other features within a format agnostic document, some relation to https://en.wikipedia.org/wiki/Literate_programming.

## Project structure 

A chapter file can reference one or more consecutive commits.

```
Chapter1.md / commit1
            / commit2
            / commit3
Chapter2.md / commit4
            / commit5
Chapter3.md / commit6
            / commit7
```

To modify a previuos chapter / fix a source code error, you would : git rebase, change the code, recompile and render - and that's it!

## Diagram / demo illustratino: 

![demo](demo.png)

## Similar projects of potential interest
https://byorgey.wordpress.com/blogliterately/
http://www.andrevdm.com/posts/2018-02-05-hakyll-code-build-include-compiler.html
http://howardism.org/Technical/Emacs/literate-devops.html

## Supported tags / features:
`{{ chapterHeader }}` returns the text `Chapter x` where x is the chapter number.

`{{ gitDiff path/to/file.sh }}` returns  a `git diff` of file.

`{{ file path/to/file.sh }}` returns the entire content of file.

`{{ fileSection path/to/file.hs main }}` like `file` but returns a section from a file (thanks to https://github.com/owickstrom/pandoc-include-code).

`{{ gitCommitOffset }}` returns a special chapter's commit 'range'.

`{{{{ shellOutput command goes here }}}}` which would execute `command goes here` (in your shell) and output whatever is returned.

```
{{{ghci optionalSessionId
:t head
4+4
}}}
```
will run the code within a GHCi session and output the results (thanks to https://github.com/byorgey/BlogLiterately). The optionalSessionId is a random string you can choose to 'persist' a GHCi process between tags.


## Limitations
- Modifying old chapters requires doing a git rebase on that project - which may present some difficulty for the usual git collaberation (as you are basically rewriting git repo history). However changes can be shared by using additional git branches.
- Not able to escape tags - so there may be issues if you use text tags like `{{example}}`.

## Installation

    stack install gitchapter:exe:gitchapter

## How do I create a project that can be rendered?
Create a `chapters` directory in a git repository. Create a `x_Example.md` file with your relevant commits, where x is a 'chapter' number. You can then render a project by executing:
`gitchapter /path/to/project` - this will generate a `compiledArticle.md` file from all the chapters.  

Also using <https://github.com/jgm/pandoc> will allow you to generate HTML from markdown with a simple command like:
`pandoc --from markdown_strict+backtick_code_blocks -s compiledArticle.md -o compiled.html`

### Example 'GitChapter' projects:

- https://github.com/chrissound/NextUpHarticle
  Output available at: https://github.com/chrissound/NextUpHarticle/blob/master/compiledArticle.md

- https://github.com/chrissound/GentleIntroductionToMonadTransformers
  Output available at: https://trycatchchris.co.uk/post/view/Hart-article-A-Gentle-Introduction-To-Monad-Transformers


## Need help?
As this is a new project, if you hit any issues or need help setting anything up - please don't hesitate to post a Github issue! :smile: 

