const path         = require('path')
const express      = require('express')
const spawn        = require('child_process').spawnSync
const bodyParser   = require('body-parser')
const cookieParser = require('cookie-parser')
const Guid         = require('guid')
const Web3         = require('web3')

const app = express()

app.use(bodyParser.json())
app.use(cookieParser())

const web3 = new Web3('https://ropsten.infura.io/v3/ce994c88153644e6ae630a577cf28b17')

class Db {
  constructor() {
    this.users = [] // { username: string, password: string, tokens: string[] }
    this.invoices = [] // { username: string, ... }
    this.wallets = [] // { username: string, address: string, secret: string, invoiceId: string }
  }

  // Adds user to DB
  addUser(user) {
    this.users.push(user)
  }

  // => bool -- is user in DB
  hasUser(username) {
    return this.users.some(user => user.username === username)
  }

  // do creds match with any user in db
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

  // returns list of invoices for given username
  async findInvoicesByUsername(username) {
    let invoices = this.invoices.filter(invoice => invoice.username === username)

    await Promise.all(invoices.map(async invoice => {
      const balance = await web3.eth.getBalance(invoice.address)
      invoice.paid = Number(balance)
      return invoice
    }))

    return invoices
  }

  async findInvoiceByInvoiceId(id) {
    let invoice = this.invoices.find(invoice => invoice.id === id)

    if (!invoice) return null

    const balance = await web3.eth.getBalance(invoice.address)
    invoice.paid = Number(balance)
    return invoice
  }

  findWalletsByUsername(username) {
    return this.wallets.filter(wallet => wallet.username === username)
  }

  createWallet(username, invoiceId) {
    const web3Wallet = web3.eth.accounts.create()

    return {
      invoiceId: invoiceId,
      username: username,
      address: web3Wallet.address,
      secret: web3Wallet.privateKey
    }
  }

  createInvoiceForUsername(username) {
    const invoiceId = Guid.raw()
    const wallet = this.createWallet(username, invoiceId)

    const newInvoice = {
      id: invoiceId,
      username: username,
      date: new Date(),        // When it's created
      total: 0,                // The initial charge
      paid: 0,                 // How much has been paid
      to: '',
      from: '',
      address: wallet.address, // The crypo wallet address
      description: ''
    }

    this.invoices.push(newInvoice)
    this.wallets.push(wallet)
    return newInvoice
  }

  updateInvoice(invoice) {
    const newInvoices = this.invoices.filter(i => i.id !== invoice.id)
    newInvoices.push(invoice)
    this.invoices = newInvoices
    return newInvoices
  }
}

const db = new Db()

const generateToken = () => {
  return Guid.raw()
}

// Only allow signed in users through this route
// Use this as express middleware on routes only
// signed in users should be able to access.
const onlyUser = (redirRoute) => (req, res, next) => {
  const reject = () => {
    res.clearCookie('token') // make sure client doesn't have lingering cookies
    if (redirRoute) res.redirect(302, redirRoute)
    else res.status(401).send('Must be signed in')
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

// Serve all the static files
app.use(express.static(path.resolve(__dirname, './dist')))

app.post('/signup', (req, res) => {
  const { username, password } = req.body
  const token = generateToken()

  if (db.hasUser(username)) return res.status(409).send("Username taken")

  db.addUser({ username, password, tokens: [token] })

  res.cookie('token', token)
  res.cookie('username', username)
  res.status(201).send(token)
})

app.post('/signin', (req, res) => {
  const { username, password } = req.body

  const user = db.validateCredentials(username, password)

  if (user) {
    const token = generateToken()
    user.tokens.push(token)
    res.cookie('token', token)
    res.cookie('username', username)

    return res.status(201).json({ token })
  } else {
    return res.status(401).json({ message: 'Invalid credentials' })
  }
})

app.get('/signout', onlyUser('/signin'), (req, res) => {
  const token = req.cookies.token
  const user = req.user

  // invalidate this token in the db
  user.tokens = user.tokens.filter(t => t !== token)

  // nix the token from the cookies
  res.clearCookie('token')
  res.clearCookie('username')

  res.redirect('/')
})

app.get('/invoices', onlyUser('/signin'), async (req, res) => {
  const user = req.user

  const invoices = await db.findInvoicesByUsername(user.username)

  res.format({
    json: () => {
      res.status(200).json(invoices)
    },

    html: () => {
      res.sendFile(path.resolve(__dirname, './index.html'))
    }
  })
})

app.get('/invoices/:id', async (req, res) => {
  const user = req.user

  res.format({
    json: async () => {
      const invoice = await db.findInvoiceByInvoiceId(req.params.id)
      if (invoice) res.status(200).json(invoice)
      else         res.status(404).json()
    },

    html: () => {
      res.sendFile(path.resolve(__dirname, './index.html'))
    }
  })
})

app.get('/wallets', onlyUser("/signin"), (req, res) => {
  const user = req.user

  res.format({
    json: () => {
      const wallets = db.findWalletsByUsername(user.username)
      res.json(wallets)
    },

    html: () => {
      res.sendFile(path.resolve(__dirname, './index.html'))
    }
  })
})

// create invoice
app.post('/create', onlyUser(), (req, res) => {
  const user = req.user

  const invoice = db.createInvoiceForUsername(user.username)

  res.json(invoice)
})

app.put('/update', onlyUser(), (req, res) => {
  const user = req.user
  const invoice = req.body

  if (invoice.username !== user.username) return res.status(401).send()

  db.updateInvoice(invoice)

  return res.json(invoice)
})

// Serve the html
app.get(['/', '/signin', '/invoices', '/invoices/:id', '/wallets'], (req, res) => {
  res.sendFile(path.resolve(__dirname, './index.html'))
})

// Fire up the serving!
app.listen(4444, console.log.bind(console, 'App listening on port 4444!'))
