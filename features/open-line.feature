Feature: Open line above and below.
  In order to do something
  As a user
  I want to do something

  Scenario: Create new line below
    Given I am in "fundamental" mode
    And I clear the buffer
    Given I insert:
    """
    This is some text
    """
    And I go to point "5"
    And I press "C-o"
    Then the cursor should be at point "19"
    Then I should see:
    """
    This is some text

    """

  Scenario: Create new line above
    Given I am in "fundamental" mode
    And I clear the buffer
    Given I insert:
    """
    This is some text
    """
    And I go to point "5"
    And I press "C-S-o"
    Then the cursor should be at point "5"
    Then I should see:
    """

    This is some text
    """
