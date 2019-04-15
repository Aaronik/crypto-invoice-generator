const path         = require('path')
const express      = require('express')
const spawn        = require('child_process').spawnSync
const bodyParser   = require('body-parser')
const cookieParser = require('cookie-parser')
const Guid         = require('guid')

class Db {
  constructor() {
    this.users = [] // { username: string, password: string, tokens: string[] }
    this.invoices = [] // { username: string, ... }
  }

  addUser(user) {
    this.users.push(user)
  }

  validateCredentials(username, password) {
    return this.users.find(user => {
      return user.username === username && user.password === password
    })
  }

  findUserByToken(token) {
    return this.users.find(user => {
      return user.tokens.some(t => t === token)
    })
  }

  findInvoicesByUsername(username) {
    return this.invoices.filter(invoice => invoice.username === username)
  }

  createInvoiceForUsername(username) {
    const newInvoice = {
      id: Guid.raw(),
      username: username,
      date: new Date(),       // When it's created
      total: 0,               // The initial charge
      paid: 0,                // How much has been paid
      to: '',
      from: '',
      address: '',            // The crypo wallet address
      description: ''
    }

    this.invoices.push(newInvoice)
    return newInvoice
  }
}

const db = new Db()

const generateToken = () => {
  return Guid.raw()
}

// Only allow signed in users through this route
// Use this as express middleware on routes only
// signed in users should be able to access.
const onlyUser = (req, res, next) => {
  const reject = () => {
    res.clearCookie('token') // make sure client doesn't have lingering cookies
    res.redirect(302, '/signin') // redirect to signin
  }

  const token = req.cookies.token

  if (!token) return reject()

  const user = db.findUserByToken(token)
  // If the user is not signed in, we'll reject em
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

app.post('/signup', (req, res) => {
  // TODO handle duplicate usernames
  console.log("/signup", req.body)

  const { username, password } = req.body
  const token = generateToken()

  db.addUser({ username, password, tokens: [token] })

  res.cookie('token', token)
  res.status(201).send(token)
})

app.post('/signin', (req, res) => {
  console.log("/signin", req.body)

  const { username, password } = req.body

  const user = db.validateCredentials(username, password)

  if (user) {
    const token = generateToken()
    user.tokens.push(token)
    res.cookie('token', token)

    return res.status(201).json({ token })
  } else {
    return res.status(401).json({ message: 'Invalid credentials' })
  }
})

app.get('/signout', onlyUser, (req, res) => {
  const token = req.cookies.token
  const user = req.user

  console.log('/signout:', user)

  // invalidate this token in the db
  user.tokens = user.tokens.filter(t => t !== token)

  // nix the token from the cookies
  res.clearCookie('token')

  res.redirect('/')
})

app.get('/invoices', onlyUser, (req, res) => {
  const user = req.user

  const invoices = db.findInvoicesByUsername(user.username)

  res.format({
    json: () => {
      res.status(200).json(invoices)
    },

    html: () => {
      res.sendFile(path.resolve(__dirname, './index.html'))
    }
  })
})

app.post('/create', onlyUser, (req, res) => {
  const user = req.user

  console.log('/create', user)

  const invoice = db.createInvoiceForUsername(user.username)

  res.json(invoice)

})

// Serve the html
app.get(['/', '/signin', '/invoices', '/invoices/:id'], (req, res) => {
  console.log(req.params)
  res.sendFile(path.resolve(__dirname, './index.html'))
})

// Fire up the serving!
app.listen(4444, console.log.bind(console, 'App listening on port 4444!'))
