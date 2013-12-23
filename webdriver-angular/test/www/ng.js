angular.module("test", [])
.controller("TestCtrl", function($scope) {
    $scope.a = "A";
    $scope.dogs = [{name:"Spot", breed:"mutt"},
                   {name:"Spike", breed:"poodle"},
                   {name:"Jupiter", breed:"bulldog"}];
    $scope.cost = 12.6;
});
