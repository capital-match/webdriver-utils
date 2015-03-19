## 1.0.2

* Fix so that the package builds against webdriver 0.6.1

## 1.0.1

* Increase upper bound on hspec to allow hspec 2.1

## 1.0.0

* Update to hspec2.  All the same features are present but some of the types changed
  to reflect hspec2 types.

## 0.3.2

* Update to the new hspec 1.12 API

## 0.3.1

* Convert inspectSession to turn into a pending example instead of throwing an error.

## 0.3.0

* Add support for testing multiple sessions at once
    * multiSession, multiSessionWith, and runWDWith are the new functions
    * the type of runWD and WDExample changed

* Update to hspec 0.11 and webdriver 0.6
    * changed sessionOn to sessionWith to work with new webdriver WDConfig
    * TestCapabilities(newCaps) changed type to @c -> IO W.Capabilities@

## 0.2.3

* Add inspectSession to assist debugging the test suite

## 0.2.2

* Allow newer version of hspec

## 0.2.1

* Allow newer version of hspec

## 0.2.0

* Convert to use webdriver sessions

## 0.1.0

* Initial Release
