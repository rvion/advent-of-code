'use strict'
exports.debug = function(a){
  return function(){
    console.log(a)
  }
}
exports.md5 = require('md5')
