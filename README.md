# Freelancer-Payment-Escrow
 
# Freelancer Payment Escrow (FPE)

A decentralized escrow system for freelance work payments built on Stacks blockchain.

## Overview

FPE provides a trustless environment for freelancers and employers to conduct business. It ensures that:
- Employers' funds are safely locked until work is completed
- Freelancers are guaranteed payment for approved work
- Disputes can be resolved by a designated mediator

## Contract Functions

### For Employers

- `create-contract`: Create a new contract by locking STX
- `release-payment`: Release payment to freelancer after work approval

### For Freelancers

- `accept-contract`: Accept a pending contract
- `complete-work`: Mark work as completed

### For Both Parties

- `raise-dispute`: Raise a dispute for resolution
- `get-contract`: View contract details
- `get-dispute`: View dispute details

### For Mediator

- `resolve-dispute`: Resolve disputes by releasing payment to either party
- `set-mediator`: Update mediator address

## Usage Flow

1. Employer creates contract with locked funds
2. Freelancer accepts contract
3. Freelancer completes work and marks it
4. Employer reviews and releases payment
   - Or raises dispute if issues exist
   - Mediator resolves disputes if needed

## Testing

Use Clarinet to run tests:
```bash
clarinet test
```

## Deployment

Deploy using Clarinet:
```bash
clarinet deploy
```