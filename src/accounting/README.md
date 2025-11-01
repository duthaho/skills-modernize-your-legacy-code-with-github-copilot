# Account Management System - Node.js Implementation

This is a modernized Node.js implementation of the legacy COBOL Account Management System. The application preserves all original business logic, data integrity, and menu options from the COBOL version.

## Architecture

The Node.js application follows the same three-layer architecture as the original COBOL system:

### 1. DataProgram (Data Layer)
- Manages balance storage and persistence
- Provides READ and WRITE operations
- Equivalent to `data.cob`

### 2. Operations (Business Logic Layer)
- Implements core account operations
- Handles balance inquiries, credits (deposits), and debits (withdrawals)
- Enforces business rules (e.g., insufficient funds check)
- Equivalent to `operations.cob`

### 3. MainProgram (User Interface Layer)
- Displays interactive menu
- Handles user input and navigation
- Controls application flow
- Equivalent to `main.cob`

## Features

- **View Balance**: Display current account balance
- **Credit Account**: Deposit funds into the account
- **Debit Account**: Withdraw funds (with insufficient funds validation)
- **Exit**: Close the application

## Business Rules Preserved

1. **Initial Balance**: $1,000.00
2. **Balance Format**: Up to $999,999.99 (displayed with 2 decimal places)
3. **Credit Operations**: No limit on deposit amounts
4. **Debit Operations**: Cannot withdraw more than available balance
5. **Insufficient Funds**: Transactions are rejected if balance is insufficient
6. **Data Integrity**: All balance updates use read-modify-write pattern

## Installation

```bash
# Navigate to the accounting directory
cd src/accounting

# Install dependencies (if any are added in the future)
npm install
```

## Usage

### Running from Command Line

```bash
# From the src/accounting directory
node index.js

# Or using npm script
npm start
```

### Running with Debugging

```bash
npm run dev
```

### Running from VS Code

Use the built-in debugger with the launch configurations:
1. Press `F5` or go to Run > Start Debugging
2. Select "Launch Account Management System" from the dropdown
3. The application will start in the integrated terminal

## Available Launch Configurations

The `.vscode/launch.json` file includes:

1. **Launch Account Management System**: Run the Node.js application
2. **Debug Account Management System**: Run with debugging enabled
3. **Run COBOL Application**: Run the original COBOL version for comparison

## Example Usage

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 1
Current balance: 001000.00

--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 2
Enter credit amount: 500
Amount credited. New balance: 001500.00

--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 3
Enter debit amount: 300
Amount debited. New balance: 001200.00

--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 3
Enter debit amount: 2000
Insufficient funds for this debit.

--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 4
Exiting the program. Goodbye!
```

## Code Structure

```javascript
// Data Layer
class DataProgram {
  execute(operationType, balance)  // READ or WRITE operations
}

// Business Logic Layer
class Operations {
  execute(operationType, callback)  // TOTAL, CREDIT, or DEBIT
  viewBalance(callback)             // View current balance
  creditAccount(callback)           // Deposit funds
  debitAccount(callback)            // Withdraw funds
}

// User Interface Layer
class MainProgram {
  displayMenu()                     // Show menu options
  processChoice(choice)             // Handle user selection
  mainLoop()                        // Main program loop
  exit()                            // Exit application
}
```

## Testing

### Automated Tests

The application includes a comprehensive automated test suite with **49 unit tests** covering all business logic.

```bash
# Run all tests
npm test

# Run tests in watch mode
npm run test:watch

# Run tests with coverage report
npm run test:coverage
```

**Test Results**: ✅ All 49 tests passing

The test suite validates:
- Initial balance and application setup
- View balance operations
- Credit (deposit) operations
- Debit (withdrawal) operations with sufficient/insufficient funds
- Mixed transaction sequences
- Data persistence and state management
- Boundary conditions and edge cases
- Input validation
- Data integrity

For detailed test documentation, see `TESTING.md`.

### Manual Testing

The application can also be manually tested using the test plan documented in `docs/TESTPLAN.md`. All test cases from the COBOL application apply to this Node.js implementation.

## Differences from COBOL Version

### Improvements
- **Better Error Handling**: Validates numeric input for amounts
- **Modern JavaScript**: Uses ES6 classes and async/await patterns
- **Modular Design**: Easy to extend and test
- **Exportable Modules**: Can be imported for unit testing

### Maintained Compatibility
- Same menu structure and options
- Same balance formatting (PIC 9(6)V99 equivalent)
- Same business logic and validation rules
- Same user interaction flow

## Future Enhancements

Consider these improvements for production use:
- Add unit tests (Jest, Mocha, or similar)
- Add integration tests
- Implement database persistence
- Add transaction history
- Support multiple accounts
- Add authentication
- Create REST API endpoints
- Add logging and monitoring
- Implement configuration management

## Migration Notes

This Node.js implementation was created from the COBOL codebase with the following mapping:

| COBOL File | Node.js Class | Purpose |
|------------|---------------|---------|
| `data.cob` | `DataProgram` | Data persistence layer |
| `operations.cob` | `Operations` | Business logic operations |
| `main.cob` | `MainProgram` | User interface and flow control |

All business rules documented in `docs/README.md` have been preserved in this implementation.

## License

MIT

## Contributing

When making changes, ensure:
1. Business logic remains consistent with COBOL version
2. All test cases in `docs/TESTPLAN.md` still pass
3. Code follows modern JavaScript best practices
4. Documentation is updated accordingly
