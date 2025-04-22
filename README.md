# Decentralized Farm Crowdfunding Platform (DCP)

A Clarity smart contract that enables small farms to raise funds from backers who earn a portion of the harvest profits.

## Overview

The Decentralized Farm Crowdfunding Platform (DCP) allows:

- Farmers to create funding campaigns for their agricultural projects
- Backers to contribute STX to campaigns they want to support
- Successful campaigns to distribute profits back to backers
- Failed campaigns to refund contributions

## Contract Features

- **Campaign Creation**: Farmers can create campaigns with funding goals, deadlines, and profit-sharing percentages
- **Contributions**: Backers can fund campaigns with STX
- **Campaign Lifecycle**: Campaigns have active and ended states
- **Fund Distribution**: Successful campaigns release funds to farmers
- **Profit Sharing**: Farmers can distribute profits to backers based on contribution percentages
- **Refunds**: Failed campaigns allow backers to reclaim their contributions

## How to Use

### For Farmers

1. **Create a Campaign**:
   ```
   (contract-call? .dcp create-campaign "Organic Apple Orchard" "Help us expand our organic apple orchard" u10000000 u30000 u200)
   ```
   Parameters:
   - Title (string-ascii 100)
   - Description (string-ascii 500)
   - Funding goal in microSTX
   - Block height deadline
   - Profit percentage (in basis points, e.g., 200 = 20%)

2. **End Campaign** (after deadline):
   ```
   (contract-call? .dcp end-campaign u1)
   ```

3. **Withdraw Funds** (if funding goal reached):
   ```
   (contract-call? .dcp withdraw-funds u1)
   ```

4. **Distribute Profits** (after harvest):
   ```
   (contract-call? .dcp distribute-profit u1 u5000000)
   ```
   - Campaign ID
   - Profit amount in microSTX

### For Backers

1. **Contribute to a Campaign**:
   ```
   (contract-call? .dcp contribute u1 u1000000)
   ```
   - Campaign ID
   - Contribution amount in microSTX

2. **Claim Profit Share** (after profit distribution):
   ```
   (contract-call? .dcp claim-profit u1)
   ```

3. **Request Refund** (if campaign fails):
   ```
   (contract-call? .dcp refund u1)
   ```

### For Platform Administrators

1. **Set Platform Fee**:
   ```
   (contract-call? .dcp set-platform-fee u50)
   ```
   - Fee in basis points (e.g., 50 = 5%)

## Read-Only Functions

- `get-platform-fee`: Returns the current platform fee
- `get-campaign`: Returns details about a specific campaign
- `get-contribution`: Returns a user's contribution to a campaign
- `get-campaign-contributors`: Returns the list of contributors to a campaign

## Development

This contract is built for the Stacks blockchain using Clarity and can be deployed using Clarinet.
