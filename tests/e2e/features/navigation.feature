Feature: Navigation
            As a funk user
    I want to define navigation
    In order to structure the information architecture of my application

    Regression test for https://github.com/funk-team/funk/issues/618

Scenario: Basic in-app navigation

    # log in & create project
    Given that I open the app
    Then I see the editor

    # draw screens and add content
    When I use the shortcut Shift-E
    And I draw a screen with an element
    And add the text screen1 to the currently selected element

    And I draw another screen with an element
    And add the text screen2 to the currently selected element

    # define navigation
    Then I select the element that says screen1
    And tell the element to navigate to the second screen

    # test
    When I click the first preview button
    Then I expect to see screen1

    When I click the link that says screen1
    Then I expect to see screen2
