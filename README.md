# Decentralized Social Login

This project implements a decentralized social login system using a smart contract written in Clarity for the Stacks blockchain ecosystem.

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Smart Contract Functions](#smart-contract-functions)
- [Contributing](#contributing)
- [Author](#author)

## Overview

The Decentralized Social Login project allows users to create and manage their digital identities on the Stacks blockchain. It provides a basic framework for user registration, profile management, and identity verification without relying on centralized authorities.

## Features

- User registration with username and email
- Profile updates
- Profile image management
- User information retrieval
- User registration status check
- Total user count tracking

## Prerequisites

- [Stacks Blockchain](https://www.stacks.co/) access
- [Clarity language](https://clarity-lang.org/) knowledge
- [Clarinet](https://github.com/hirosystems/clarinet) for local development and testing (optional)

## Installation

1. Clone this repository:
   ```
   git clone https://github.com/fhayvy/identity-X.git
   cd decentralized-social-login
   ```

2. Deploy the smart contract to the Stacks blockchain using your preferred method (Clarinet, Stacks transactions, etc.)

## Usage

Interact with the smart contract using Stacks transactions. You can build a front-end application that calls the contract functions to provide a user interface for the decentralized social login system.

## Smart Contract Functions

1. `register-user`: Register a new user with a username and email.
2. `update-profile`: Update an existing user's profile information.
3. `set-profile-image`: Set or update a user's profile image URL.
4. `get-user-info`: Retrieve a user's information.
5. `get-user-count`: Get the total number of registered users.
6. `is-user-registered`: Check if a user is registered.

For detailed function signatures and usage, refer to the smart contract code.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Author

Favour Chiamaka Eze