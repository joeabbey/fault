const express = require('express');
const { Router, json } = require('express');
const path = require('path');

function createApp() {
  const app = express();
  app.use(json());
  return app;
}

module.exports = { createApp };
