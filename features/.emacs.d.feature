Feature: Everything loads
  In order to start emacs
  As a busy person
  I don't want to have to do anything manually

  Scenario: Emacs has started
    Given I type "Hello there"
    Then I should see "Hello there"
