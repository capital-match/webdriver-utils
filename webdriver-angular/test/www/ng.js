angular.module("test", [])
.controller("TestCtrl", function($scope) {
    $scope.a = "A";
    $scope.dogs1 = [{name:"Spot", breed:"mutt"},
                   {name:"Spike", breed:"poodle"},
                   {name:"Jupiter", breed:"bulldog"}];
    $scope.dogs2 = angular.copy($scope.dogs1);
    $scope.cost = 12.6;
});
