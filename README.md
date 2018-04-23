# GitChapter

## What problem does this solve?
Allows you to embed and reference various features within a format agnostic document, some relation to https://en.wikipedia.org/wiki/Literate_programming.

A project would be structured as:

```
Chapter1.md / commit1
            / commit2
            / commit3
Chapter2.md / commit4
            / commit5
Chapter3.md / commit6
            / commit7
```

This also means that if you (author) want to `amend` an old chapter / code, you do a git rebase, change the code, recompile, and render - and that's it! No need to modify anything else as the chapter files only store references relating to files / diffs.

## Diagram / demo illustratino: 

![demo](demo.png)

## Similar projects of potential interest
https://byorgey.wordpress.com/blogliterately/
http://www.andrevdm.com/posts/2018-02-05-hakyll-code-build-include-compiler.html
http://howardism.org/Technical/Emacs/literate-devops.html

## Supported tags / features:
`{{ chapterHeader }}` returns the text `Secton x` where x is the chapter number.

`{{ gitDiff path/to/file.sh }}` returns  a `git diff` of file.

`{{ file path/to/file.sh }}` returns the entire content of file.

`{{ gitCommitOffset }}` returns a special chapter's commit 'range'.

`{{ fileSection path/to/file.hs main }}` returns a section from a file (thanks to https://github.com/owickstrom/pandoc-include-code).

`{{{{ shellOutput command goes here }}}}` which would execute `command goes here` (in your shell) and output whatever is returned.

```
{{{ghci optionalSessionId
:t head
4+4
}}}
``` will run the code within a GHCi session and output the results (thanks to https://github.com/byorgey/BlogLiterately). The optionalSessionId is a random string you can choose to 'persist' a GHCi process between tags.


## Limitations
- Modifying 'older' chapter requires doing a  git rebase on that project - which may present some difficulty if you are collaborating with from others. However changes can be shared by using additional git branches.
- There may be issues if you use text tags like `{{example}}` - there isn't any way to escape these at the moment. (TODO!)

## Installation
You would need to install GHC (the Haskell compiler) + stack. Will eventually provide a binary / docker image. 

## How to use this? Instructions?
Create a `chapters` directory in a git repository. Create a `x_Example.md` file with your relevant commits, where x is a 'chapter' number. You can then render a project by executing:
`stack exec app -- /path/to/project` - this will generate a `compiledArticle.md` file from all the chapters.  

Also using <https://github.com/jgm/pandoc> will allow you to generate HTML from markdown with a simple command like:
`pandoc --from markdown_strict+backtick_code_blocks -s compiledArticle.md -o compiled.html`

### Example 'GitChapter' projects:

- https://github.com/chrissound/NextUpHarticle
  Output available at: https://github.com/chrissound/NextUpHarticle/blob/master/compiledArticle.md

- https://github.com/chrissound/GentleIntroductionToMonadTransformers
  Output available at: https://trycatchchris.co.uk/post/view/Hart-article-A-Gentle-Introduction-To-Monad-Transformers


## Need help?
As this is a new project, if you hit any issues or need help setting anything up - please don't hesitate to post a Github issue! :smile: 

