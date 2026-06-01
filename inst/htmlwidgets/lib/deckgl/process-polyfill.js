// Polyfill for Node.js 'process' global variable
// Required by some bundled dependencies that expect Node.js environment
(function() {
  if (typeof process === 'undefined') {
    window.process = {
      env: {},
      version: '',
      versions: {},
      browser: true,
      nextTick: function(fn) {
        setTimeout(fn, 0);
      }
    };
  }
  
})();
