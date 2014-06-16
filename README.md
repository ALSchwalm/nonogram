
nonogram.el
===========

nonogram.el provides an implimentation of [nonograms](https://en.wikipedia.org/wiki/Nonogram) for emacs.

Intallation
===========

To try out nonogram.el, first clone the project to a directory and then run `M-x load-file RET` and enter `path/to/nonogram.el`. To generate a puzzle and start solving, execute `M-x nonogram`. By default the puzzle is generated on a 10x10 grid.

Usage
=====

Controls consist of the typical emacs movement keys (i.e. `C-f`, `C-b`, `C-p`, `C-n`), as well as `C-e` and `C-a` for line movement. Additionally, `space` may be used to mark a box as filled, and `j` to mark a box as empty.

License
=======

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.
