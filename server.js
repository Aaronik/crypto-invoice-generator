const path         = require('path')
const express      = require('express')
const spawn        = require('child_process').spawnSync
const bodyParser   = require('body-parser')
const cookieParser = require('cookie-parser')
const Guid         = require('guid')

let db = {
  users: [],   // { username: string, password: string, tokens: string[] }
  invoices: []
}

const validateCredentials = (username, password) => {
  return db.users.find(user => {
    return user.username === username && user.password === password
  })
}

const generateToken = () => {
  return Guid.raw()
}

// Only allow signed in users through this route
// Use this as express middleware on routes only
// signed in users should be able to access.
const onlyUser = (req, res, next) => {
  const reject = () => res.status(401).send()

  const token = req.cookies.token

  if (!token) return reject()

  const user = db.users.find(user => {
    return user.tokens.some(t => t === token)
  })

  // If the user is not signed in, we'll send em a 401
  if (!user) return reject()

  // If they are, we'll attach their user object to req
  req.user = user
  next()
}

const app = express()

app.use(bodyParser.json())
app.use(cookieParser())

// Serve all the static files
app.use(express.static(path.resolve(__dirname, './dist')))

// Serve the html
app.get('/', (req, res) => {
  res.sendFile(path.resolve(__dirname, './index.html'))
})

app.post('/signup', (req, res) => {
  console.log("/signup", req.body)

  const { username, password } = req.body
  const token = generateToken()

  db.users.push({ username, password, tokens: [token] })

  res.cookie('token', token)
  res.status(201).send(token)
})

app.post('/signin', (req, res) => {
  console.log("/signin", req.body)

  const { username, password } = req.body

  const user = validateCredentials(username, password)

  if (user) {
    const token = generateToken()
    user.tokens.push(token)

    return res.status(201).json({ token })
  } else {
    return res.status(401).json({ message: 'Invalid credentials' })
  }

  res.cookie('token', token)
  res.status(201).json({})
})

app.get('/signout', onlyUser, (req, res) => {
  const token = req.cookies.token
  const user = req.user

  // invalidate this token in the db
  user.tokens = user.tokens.filter(t => t !== token)

  // nix the token from the cookies
  res.clearCookie('token')

  res.status(200).json({})
})

// Fire up the serving!
app.listen(4444, console.log.bind(console, 'App listening on port 4444!'))