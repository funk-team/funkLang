Feature: Design applications

    As a funk user
    I want to add icons to my elements
    In order to improve visual interest and grab the user's attention


Scenario: Use a google font
    Given that I open the app
    Then I see the editor

    # draw screens and add content
    Then I use the shortcut Shift-E
    And I draw a screen with an element

    And add the text screen1 to the currently selected element


    Then I click the Style tab
    And I add a custom typography style

    Then I set the font family to Poppins
    And I expect the font family to be honored


Scenario: Add icon from GitHub and select the element
    Given that I open the app
    Then I see the editor

    # draw screens and add content
    Then I use the shortcut Shift-E
    And I draw a screen with an element

    When I go to Design mode
    And go to icons

    # select an icon that covers the element in order to test the regression in
    # https://github.com/funk-team/funk/issues/627
    And select an icon that covers the center of its viewbox
    Then I wait for the icon to show up in my custom icon set

    Then I add the icon to the currently selected element
    And I draw another screen with an element

    When I click the element with the icon
    Then I expect the element to be selected

Scenario: Draw and move elements around
    Given that I open the app
    Then I see the editor

    # draw screens and add content
    Then I use the shortcut Shift-E
    And I draw a screen with an element

    And I select the select and transform tool
    When I click the screen
    And I click and drag the screens's center handle
    Then I expect the screen to still be selected
    And I expect the screen to be in a different location
