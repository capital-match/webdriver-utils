For end to end testing of web applications from Haskell, the
[webdriver](https://hackage.haskell.org/package/webdriver) package is a great tool but does not
provide specific commands to make testing a webpage using [AngularJs](http://angularjs.org/) easier.
The [protractor](https://github.com/angular/protractor) project provides Angular-specific webdriver
commands but the test code must be written in javascript.  This package fills the gap by reusing
some of the protractor code to allow end to end tests of Angular applications to be written in
Haskell.
