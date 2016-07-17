# purescript-checkers

Checkers implemented in PureScript.

## Installation (Mac OS X)

Install npm:

* `sudo port install nodejs` or `sudo brew install node`

Install PureScript:

* `npm install -g purescript@0.8.5`

Install pulp:

* `npm install -g pulp@8.2.1`

Install bower:

* `npm install -g bower`

Install dependencies:

* `bower update`

Compile PureScript to JavaScript:

* `pulp browserify -m Checkers.Main > html/checkers.js`

Open page in browser:

* `open html/index.html`
