const path       = require('path')
const express    = require('express')
const spawn      = require('child_process').spawnSync
const bodyParser = require('body-parser')

const app = express()

app.use(bodyParser.json())

// Serve all the static files
app.use(express.static(path.resolve(__dirname, './dist')))

// Serve the html
app.get('/', (req, res) => {
  res.sendFile(path.resolve(__dirname, './index.html'))
})

// Fire up the serving!
app.listen(4444, console.log.bind(console, 'App listening on port 4444!'))
