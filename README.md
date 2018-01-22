# Hart

## What problem does this solve?
Makes it easy to write technical article  while including files as annotations. Allows you to associate a file (format agnostic) with a set of commits within a git repository.

So something along the lines of
```
Section1.md / commit1
            / commit2
            / commit3
Section2.md / commit4
            / commit5
Section3.md / commit6
            / commit7
```

This also means that if you want to `amend` an old section / code, you do a git rebase, change the code and thats it (well you still need to rerun the compilation / rendering)! No need to modify anything else as the section files only store references relating to files / diffs.


## Supported tags / features:

Format agnostic - although there may be references to using markdown (.md), it should work on any type of text.

`{{sectionHeader}}` which would be `Secton x` where x is the section number.

`{{ gitDiff path/to/file.sh }}` which would show you a `git diff` of file relative to the section's 'parent commit'.

`{{ file path/to/file.sh }}` which would show you entire content of file relative to the section's 'parent commit'.

`{{gitCommitOffset}}` will output the seciton's commit 'range'.

`{{{{ shellOutput command goes here }}}}` which would execute `command goes here` (in your shell) and output whatever is returned.


## Limitations
Not 100% tested - but the core functionality (secitons?) works!
- Modifying 'older' sections means needing to do a git rebase on that project - which may present some difficulty if you are accepting changes (git commits) from others.
- There may be issues if you use text tags like `{{example}}` - there isn't any way to escape these at the moment (a quick fix would be to require additional {{{{ brackets}}}).

## Installation

Most probably you would need to install Haskell / GHC (the Haskell compiler) + stack. It should be possible to provide a binary / docker image for this - but I'll need to investigate and test this first. 

## How to use this? Instructions?
Create a `sections` directory in a git repository. Create a `x_Example.md` file with your relevent commits, where x is a 'section' number. You can then render a project by executing:
`stack exec app -- /path/to/project` - this will generate a `compiledArticle.md` file from all the sections.  

Also using <https://github.com/jgm/pandoc> will allow you to generate HTML from markdown with a simple command like:
`pandoc --from markdown_strict+backtick_code_blocks -s compiledArticle.md -o compiled.html`

### Example 'project'

https://github.com/chrissound/NextUpHarticle

Output available at:
https://github.com/chrissound/NextUpHarticle/blob/master/compiledArticle.md

![Screenshot](demo.jpg)

## Need help?
As this is a new project, if you hit any issues or need help setting anything up - please don't hesitate to post a github issue!  :)

