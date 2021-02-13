# crypto-invoice-generator

This is a small project demonstrating clientside interop with Ethereum.

## Running

Should be as simple as

```
npm install && npm run build && npm start
```

Then just visit `localhost:4444`.

## The Idea

The app is written in Node.js (JavaScript) and Elm. It uses Bulma as a CSS framework.
I rolled my own signin / session system just b/c it'd been a minute since I'd done it
manually, and I wanted to see what it was like again. I didn't have time to set up
a proper DB, so it stores everything in memory. There's very little security so please
don't use any sensitive information. For example, passwords are stored in plaintext,
and ETH private keys are displayed on the screen. The app uses the Ropsten Ethereum testnet.
