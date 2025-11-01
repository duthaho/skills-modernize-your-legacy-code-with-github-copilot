#!/usr/bin/env node

/**
 * Account Management System - Node.js Implementation
 * 
 * This application is a modernized version of the legacy COBOL account management system.
 * It preserves the original business logic, data integrity, and menu options.
 * 
 * Architecture:
 * - DataProgram: Data persistence layer (mimics data.cob)
 * - Operations: Business logic for account operations (mimics operations.cob)
 * - MainProgram: User interface and menu system (mimics main.cob)
 */

const readline = require('readline');

// ============================================================================
// DATA LAYER (DataProgram - data.cob)
// ============================================================================

/**
 * DataProgram - Manages data persistence for account balance
 * Equivalent to COBOL DataProgram (data.cob)
 */
class DataProgram {
  constructor() {
    // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    this.storageBalance = 1000.00;
  }

  /**
   * Read or write balance based on operation type
   * @param {string} operationType - 'READ' or 'WRITE'
   * @param {number|null} balance - Balance value (for WRITE operation)
   * @returns {number|null} - Balance value (for READ operation) or null
   */
  execute(operationType, balance = null) {
    if (operationType === 'READ') {
      // MOVE STORAGE-BALANCE TO BALANCE
      return this.storageBalance;
    } else if (operationType === 'WRITE') {
      // MOVE BALANCE TO STORAGE-BALANCE
      this.storageBalance = balance;
      return null;
    }
    return null;
  }
}

// ============================================================================
// BUSINESS LOGIC LAYER (Operations - operations.cob)
// ============================================================================

/**
 * Operations - Contains core business logic for account operations
 * Equivalent to COBOL Operations (operations.cob)
 */
class Operations {
  constructor(dataProgram, rl) {
    this.dataProgram = dataProgram;
    this.rl = rl;
    // FINAL-BALANCE PIC 9(6)V99 VALUE 1000.00
    this.finalBalance = 1000.00;
  }

  /**
   * Execute operation based on type
   * @param {string} operationType - 'TOTAL ', 'CREDIT', or 'DEBIT '
   * @param {Function} callback - Callback to return to main menu
   */
  async execute(operationType, callback) {
    if (operationType === 'TOTAL ') {
      await this.viewBalance(callback);
    } else if (operationType === 'CREDIT') {
      await this.creditAccount(callback);
    } else if (operationType === 'DEBIT ') {
      await this.debitAccount(callback);
    }
  }

  /**
   * View current balance (TOTAL operation)
   */
  async viewBalance(callback) {
    // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    this.finalBalance = this.dataProgram.execute('READ');
    
    // DISPLAY "Current balance: " FINAL-BALANCE
    console.log(`Current balance: ${this.formatBalance(this.finalBalance)}`);
    
    callback();
  }

  /**
   * Credit account (deposit funds)
   */
  async creditAccount(callback) {
    // DISPLAY "Enter credit amount: "
    this.rl.question('Enter credit amount: ', (input) => {
      // ACCEPT AMOUNT
      const amount = parseFloat(input);
      
      if (isNaN(amount) || amount < 0) {
        console.log('Invalid amount. Please enter a positive number.');
        return callback();
      }

      // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
      this.finalBalance = this.dataProgram.execute('READ');
      
      // ADD AMOUNT TO FINAL-BALANCE
      this.finalBalance += amount;
      
      // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
      this.dataProgram.execute('WRITE', this.finalBalance);
      
      // DISPLAY "Amount credited. New balance: " FINAL-BALANCE
      console.log(`Amount credited. New balance: ${this.formatBalance(this.finalBalance)}`);
      
      callback();
    });
  }

  /**
   * Debit account (withdraw funds)
   */
  async debitAccount(callback) {
    // DISPLAY "Enter debit amount: "
    this.rl.question('Enter debit amount: ', (input) => {
      // ACCEPT AMOUNT
      const amount = parseFloat(input);
      
      if (isNaN(amount) || amount < 0) {
        console.log('Invalid amount. Please enter a positive number.');
        return callback();
      }

      // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
      this.finalBalance = this.dataProgram.execute('READ');
      
      // IF FINAL-BALANCE >= AMOUNT
      if (this.finalBalance >= amount) {
        // SUBTRACT AMOUNT FROM FINAL-BALANCE
        this.finalBalance -= amount;
        
        // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        this.dataProgram.execute('WRITE', this.finalBalance);
        
        // DISPLAY "Amount debited. New balance: " FINAL-BALANCE
        console.log(`Amount debited. New balance: ${this.formatBalance(this.finalBalance)}`);
      } else {
        // DISPLAY "Insufficient funds for this debit."
        console.log('Insufficient funds for this debit.');
      }
      
      callback();
    });
  }

  /**
   * Format balance for display (matches COBOL PIC 9(6)V99 format)
   * @param {number} balance - Balance to format
   * @returns {string} - Formatted balance
   */
  formatBalance(balance) {
    return balance.toFixed(2).padStart(9, '0');
  }
}

// ============================================================================
// USER INTERFACE LAYER (MainProgram - main.cob)
// ============================================================================

/**
 * MainProgram - Entry point and user interface
 * Equivalent to COBOL MainProgram (main.cob)
 */
class MainProgram {
  constructor() {
    // Create readline interface for user input
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });

    // Initialize data and operations layers
    this.dataProgram = new DataProgram();
    this.operations = new Operations(this.dataProgram, this.rl);
    
    // CONTINUE-FLAG PIC X(3) VALUE 'YES'
    this.continueFlag = 'YES';
  }

  /**
   * Display main menu
   */
  displayMenu() {
    console.log('--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
  }

  /**
   * Process user choice
   * @param {string} choice - User's menu selection
   */
  async processChoice(choice) {
    // ACCEPT USER-CHOICE
    const userChoice = parseInt(choice);

    // EVALUATE USER-CHOICE
    switch (userChoice) {
      case 1:
        // CALL 'Operations' USING 'TOTAL '
        await this.operations.execute('TOTAL ', () => this.mainLoop());
        break;
      case 2:
        // CALL 'Operations' USING 'CREDIT'
        await this.operations.execute('CREDIT', () => this.mainLoop());
        break;
      case 3:
        // CALL 'Operations' USING 'DEBIT '
        await this.operations.execute('DEBIT ', () => this.mainLoop());
        break;
      case 4:
        // MOVE 'NO' TO CONTINUE-FLAG
        this.continueFlag = 'NO';
        this.exit();
        break;
      default:
        // DISPLAY "Invalid choice, please select 1-4."
        console.log('Invalid choice, please select 1-4.');
        this.mainLoop();
        break;
    }
  }

  /**
   * Main program loop
   * PERFORM UNTIL CONTINUE-FLAG = 'NO'
   */
  mainLoop() {
    if (this.continueFlag === 'NO') {
      return;
    }

    this.displayMenu();
    this.rl.question('Enter your choice (1-4): ', (choice) => {
      this.processChoice(choice);
    });
  }

  /**
   * Exit the application
   */
  exit() {
    // DISPLAY "Exiting the program. Goodbye!"
    console.log('Exiting the program. Goodbye!');
    this.rl.close();
    // STOP RUN
    process.exit(0);
  }

  /**
   * Start the application
   */
  start() {
    console.log('\n');
    this.mainLoop();
  }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

// Only run if this file is executed directly (not imported as a module)
if (require.main === module) {
  const app = new MainProgram();
  app.start();
}

// Export for testing purposes
module.exports = {
  DataProgram,
  Operations,
  MainProgram
};
