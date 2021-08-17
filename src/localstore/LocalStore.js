"use strict";

exports.putL = function(key) {
    return function(value) {
        return function() {
            console.log(`Storing ${key} with ${value}`);
            return window.localStorage.setItem(key, value);
        };
    };
};

exports.getL = function(key) {
    return function() {
        return window.localStorage.getItem(key);
    };
};

exports.delL = function(key) {
    return function() {
        console.log(`Deleting ${key}`);
        return window.localStorage.removeItem(key);
    }
}