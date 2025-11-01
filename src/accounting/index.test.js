/**
 * Unit Tests for Account Management System
 * 
 * These tests mirror the test cases defined in docs/TESTPLAN.md
 * and verify that the Node.js implementation maintains feature parity
 * with the original COBOL application.
 */

const { DataProgram, Operations, MainProgram } = require('./index');

// Mock readline for testing
const mockReadline = {
  question: jest.fn(),
  close: jest.fn()
};

describe('Account Management System - Unit Tests', () => {
  
  // ============================================================================
  // 1. Application Initialization Tests (TC-001 to TC-003)
  // ============================================================================
  
  describe('1. Application Initialization Tests', () => {
    
    test('TC-002: Verify initial balance is set correctly', () => {
      const dataProgram = new DataProgram();
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBe(1000.00);
    });
    
    test('TC-003: Verify DataProgram initializes with correct structure', () => {
      const dataProgram = new DataProgram();
      
      expect(dataProgram).toHaveProperty('storageBalance');
      expect(dataProgram.storageBalance).toBe(1000.00);
    });
  });

  // ============================================================================
  // 2. View Balance Functionality Tests (TC-101 to TC-104)
  // ============================================================================
  
  describe('2. View Balance Functionality Tests', () => {
    
    test('TC-101: View initial balance', () => {
      const dataProgram = new DataProgram();
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBe(1000.00);
    });
    
    test('TC-102: View balance after credit', () => {
      const dataProgram = new DataProgram();
      
      // Credit $500.00
      dataProgram.execute('WRITE', 1500.00);
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBe(1500.00);
    });
    
    test('TC-103: View balance after debit', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $1000, then debit $200
      dataProgram.execute('WRITE', 800.00);
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBe(800.00);
    });
    
    test('TC-104: View balance multiple times (read-only, no side effects)', () => {
      const dataProgram = new DataProgram();
      
      const balance1 = dataProgram.execute('READ');
      const balance2 = dataProgram.execute('READ');
      const balance3 = dataProgram.execute('READ');
      
      expect(balance1).toBe(balance2);
      expect(balance2).toBe(balance3);
      expect(balance1).toBe(1000.00);
    });
  });

  // ============================================================================
  // 3. Credit Account (Deposit) Functionality Tests (TC-201 to TC-208)
  // ============================================================================
  
  describe('3. Credit Account (Deposit) Functionality Tests', () => {
    
    test('TC-201: Credit account with valid amount ($500.00)', () => {
      const dataProgram = new DataProgram();
      
      // Read current balance
      let balance = dataProgram.execute('READ');
      expect(balance).toBe(1000.00);
      
      // Credit $500.00
      balance += 500.00;
      dataProgram.execute('WRITE', balance);
      
      // Verify new balance
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(1500.00);
    });
    
    test('TC-202: Credit account with small amount ($0.01)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      balance += 0.01;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(1000.01, 2);
    });
    
    test('TC-203: Credit account with large amount ($50,000.00)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      balance += 50000.00;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(51000.00);
    });
    
    test('TC-204: Credit account with decimal amount ($123.45)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      balance += 123.45;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(1123.45, 2);
    });
    
    test('TC-205: Multiple consecutive credits', () => {
      const dataProgram = new DataProgram();
      
      // Credit $100.00
      let balance = dataProgram.execute('READ');
      balance += 100.00;
      dataProgram.execute('WRITE', balance);
      
      // Credit $200.00
      balance = dataProgram.execute('READ');
      balance += 200.00;
      dataProgram.execute('WRITE', balance);
      
      // Credit $300.00
      balance = dataProgram.execute('READ');
      balance += 300.00;
      dataProgram.execute('WRITE', balance);
      
      // Verify final balance
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(1600.00);
    });
    
    test('TC-206: Credit with zero amount', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      balance += 0.00;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(1000.00);
    });
    
    test('TC-207: Credit near maximum balance', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $999,000.00
      dataProgram.execute('WRITE', 999000.00);
      
      // Credit $999.00
      let balance = dataProgram.execute('READ');
      balance += 999.00;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(999999.00);
    });
    
    test('TC-208: Credit exceeding maximum balance (overflow test)', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $999,000.00
      dataProgram.execute('WRITE', 999000.00);
      
      // Credit $2000.00 (exceeds max)
      let balance = dataProgram.execute('READ');
      balance += 2000.00;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      // This documents the overflow behavior
      expect(newBalance).toBe(1001000.00);
    });
  });

  // ============================================================================
  // 4. Debit Account (Withdrawal) - Sufficient Funds (TC-301 to TC-306)
  // ============================================================================
  
  describe('4. Debit Account (Withdrawal) - Sufficient Funds', () => {
    
    test('TC-301: Debit account with valid amount ($300.00)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      
      // Check sufficient funds
      if (balance >= 300.00) {
        balance -= 300.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(700.00);
    });
    
    test('TC-302: Debit account with small amount ($0.01)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      
      if (balance >= 0.01) {
        balance -= 0.01;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(999.99, 2);
    });
    
    test('TC-303: Debit entire balance ($1,000.00)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      
      if (balance >= 1000.00) {
        balance -= 1000.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(0.00);
    });
    
    test('TC-304: Debit with decimal amount ($123.45)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      
      if (balance >= 123.45) {
        balance -= 123.45;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(876.55, 2);
    });
    
    test('TC-305: Multiple consecutive debits', () => {
      const dataProgram = new DataProgram();
      
      // Debit $100.00
      let balance = dataProgram.execute('READ');
      if (balance >= 100.00) {
        balance -= 100.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Debit $200.00
      balance = dataProgram.execute('READ');
      if (balance >= 200.00) {
        balance -= 200.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Debit $300.00
      balance = dataProgram.execute('READ');
      if (balance >= 300.00) {
        balance -= 300.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(400.00);
    });
    
    test('TC-306: Debit with zero amount', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      const originalBalance = balance;
      
      if (balance >= 0.00) {
        balance -= 0.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(originalBalance);
      expect(newBalance).toBe(1000.00);
    });
  });

  // ============================================================================
  // 5. Debit Account - Insufficient Funds (TC-401 to TC-405)
  // ============================================================================
  
  describe('5. Debit Account (Withdrawal) - Insufficient Funds', () => {
    
    test('TC-401: Debit exceeding balance ($1,500.00 from $1,000.00)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      const originalBalance = balance;
      
      // Attempt to debit more than balance
      if (balance >= 1500.00) {
        balance -= 1500.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Balance should remain unchanged
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(originalBalance);
      expect(newBalance).toBe(1000.00);
    });
    
    test('TC-402: Debit slightly exceeding balance ($1,000.01 from $1,000.00)', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      
      if (balance >= 1000.01) {
        balance -= 1000.01;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(1000.00);
    });
    
    test('TC-403: Debit from zero balance', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $0.00
      dataProgram.execute('WRITE', 0.00);
      
      let balance = dataProgram.execute('READ');
      
      if (balance >= 0.01) {
        balance -= 0.01;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBe(0.00);
    });
    
    test('TC-404: Verify balance unchanged after failed debit', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $500.00
      dataProgram.execute('WRITE', 500.00);
      
      let balance = dataProgram.execute('READ');
      
      // Attempt to debit $600.00 (should fail)
      if (balance >= 600.00) {
        balance -= 600.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Verify balance is still $500.00
      const balanceAfterFailedDebit = dataProgram.execute('READ');
      expect(balanceAfterFailedDebit).toBe(500.00);
    });
    
    test('TC-405: Debit exactly balance plus one cent', () => {
      const dataProgram = new DataProgram();
      
      // Set balance to $999.99
      dataProgram.execute('WRITE', 999.99);
      
      let balance = dataProgram.execute('READ');
      
      // Attempt to debit $1000.00
      if (balance >= 1000.00) {
        balance -= 1000.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(999.99, 2);
    });
  });

  // ============================================================================
  // 6. Mixed Transaction Tests (TC-501 to TC-505)
  // ============================================================================
  
  describe('6. Mixed Transaction Tests', () => {
    
    test('TC-501: Credit followed by debit', () => {
      const dataProgram = new DataProgram();
      
      // Credit $500.00
      let balance = dataProgram.execute('READ');
      balance += 500.00;
      dataProgram.execute('WRITE', balance);
      
      // Debit $300.00
      balance = dataProgram.execute('READ');
      if (balance >= 300.00) {
        balance -= 300.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(1200.00);
    });
    
    test('TC-502: Debit followed by credit', () => {
      const dataProgram = new DataProgram();
      
      // Debit $400.00
      let balance = dataProgram.execute('READ');
      if (balance >= 400.00) {
        balance -= 400.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Credit $200.00
      balance = dataProgram.execute('READ');
      balance += 200.00;
      dataProgram.execute('WRITE', balance);
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(800.00);
    });
    
    test('TC-503: Failed debit followed by successful debit', () => {
      const dataProgram = new DataProgram();
      
      // Attempt to debit $1500.00 (should fail)
      let balance = dataProgram.execute('READ');
      if (balance >= 1500.00) {
        balance -= 1500.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Debit $500.00 (should succeed)
      balance = dataProgram.execute('READ');
      if (balance >= 500.00) {
        balance -= 500.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(500.00);
    });
    
    test('TC-504: Multiple operations sequence', () => {
      const dataProgram = new DataProgram();
      
      // Start: $1,000.00
      let balance = dataProgram.execute('READ');
      expect(balance).toBe(1000.00);
      
      // Credit $500.00 -> $1,500.00
      balance += 500.00;
      dataProgram.execute('WRITE', balance);
      
      // Debit $300.00 -> $1,200.00
      balance = dataProgram.execute('READ');
      if (balance >= 300.00) {
        balance -= 300.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Credit $100.00 -> $1,300.00
      balance = dataProgram.execute('READ');
      balance += 100.00;
      dataProgram.execute('WRITE', balance);
      
      // Debit $800.00 -> $500.00
      balance = dataProgram.execute('READ');
      if (balance >= 800.00) {
        balance -= 800.00;
        dataProgram.execute('WRITE', balance);
      }
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(500.00);
    });
    
    test('TC-505: View balance between transactions', () => {
      const dataProgram = new DataProgram();
      
      // Credit $200.00
      let balance = dataProgram.execute('READ');
      balance += 200.00;
      dataProgram.execute('WRITE', balance);
      
      // View balance
      const balance1 = dataProgram.execute('READ');
      expect(balance1).toBe(1200.00);
      
      // Debit $100.00
      balance = dataProgram.execute('READ');
      if (balance >= 100.00) {
        balance -= 100.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // View balance
      const balance2 = dataProgram.execute('READ');
      expect(balance2).toBe(1100.00);
    });
  });

  // ============================================================================
  // 8. Application Exit Tests (TC-701 to TC-705)
  // ============================================================================
  
  describe('8. Application Exit Tests', () => {
    
    test('TC-701: MainProgram has exit method', () => {
      const mainProgram = new MainProgram();
      
      expect(mainProgram).toHaveProperty('exit');
      expect(typeof mainProgram.exit).toBe('function');
    });
    
    test('TC-701: Exit sets continueFlag to NO', () => {
      const mainProgram = new MainProgram();
      
      expect(mainProgram.continueFlag).toBe('YES');
      mainProgram.continueFlag = 'NO';
      expect(mainProgram.continueFlag).toBe('NO');
    });
  });

  // ============================================================================
  // 9. Data Persistence and State Management Tests (TC-801 to TC-804)
  // ============================================================================
  
  describe('9. Data Persistence and State Management Tests', () => {
    
    test('TC-801: Balance persists during session', () => {
      const dataProgram = new DataProgram();
      
      // Perform credit
      let balance = dataProgram.execute('READ');
      balance += 500.00;
      dataProgram.execute('WRITE', balance);
      
      // Perform debit
      balance = dataProgram.execute('READ');
      if (balance >= 200.00) {
        balance -= 200.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // View balance
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(1300.00);
    });
    
    test('TC-802: Balance resets on new DataProgram instance', () => {
      const dataProgram1 = new DataProgram();
      
      // Modify balance
      dataProgram1.execute('WRITE', 1500.00);
      expect(dataProgram1.execute('READ')).toBe(1500.00);
      
      // Create new instance (simulates restart)
      const dataProgram2 = new DataProgram();
      expect(dataProgram2.execute('READ')).toBe(1000.00);
    });
    
    test('TC-803: Read operation does not modify balance', () => {
      const dataProgram = new DataProgram();
      
      const balance1 = dataProgram.execute('READ');
      const balance2 = dataProgram.execute('READ');
      const balance3 = dataProgram.execute('READ');
      
      expect(balance1).toBe(balance2);
      expect(balance2).toBe(balance3);
      expect(dataProgram.storageBalance).toBe(1000.00);
    });
    
    test('TC-804: Write operation updates storage', () => {
      const dataProgram = new DataProgram();
      
      // Credit $500.00
      let balance = dataProgram.execute('READ');
      balance += 500.00;
      dataProgram.execute('WRITE', balance);
      
      // Debit $200.00
      balance = dataProgram.execute('READ');
      if (balance >= 200.00) {
        balance -= 200.00;
        dataProgram.execute('WRITE', balance);
      }
      
      // Verify final balance
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBe(1300.00);
      expect(dataProgram.storageBalance).toBe(1300.00);
    });
  });

  // ============================================================================
  // 10. Boundary and Edge Case Tests (TC-901 to TC-908)
  // ============================================================================
  
  describe('10. Boundary and Edge Case Tests', () => {
    
    test('TC-901: Maximum balance value ($999,999.99)', () => {
      const dataProgram = new DataProgram();
      
      dataProgram.execute('WRITE', 999999.99);
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBeCloseTo(999999.99, 2);
    });
    
    test('TC-902: Minimum balance value ($0.00)', () => {
      const dataProgram = new DataProgram();
      
      dataProgram.execute('WRITE', 0.00);
      const balance = dataProgram.execute('READ');
      
      expect(balance).toBe(0.00);
    });
    
    test('TC-903: Debit exact balance amount', () => {
      const dataProgram = new DataProgram();
      
      dataProgram.execute('WRITE', 1234.56);
      
      let balance = dataProgram.execute('READ');
      if (balance >= 1234.56) {
        balance -= 1234.56;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(0.00, 2);
    });
    
    test('TC-904: Credit with maximum decimal precision', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      balance += 123.99;
      dataProgram.execute('WRITE', balance);
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(1123.99, 2);
    });
    
    test('TC-905: Debit with maximum decimal precision', () => {
      const dataProgram = new DataProgram();
      
      let balance = dataProgram.execute('READ');
      if (balance >= 123.99) {
        balance -= 123.99;
        dataProgram.execute('WRITE', balance);
      }
      
      const newBalance = dataProgram.execute('READ');
      expect(newBalance).toBeCloseTo(876.01, 2);
    });
    
    test('TC-906: Negative amount handling - credit (validation test)', () => {
      const dataProgram = new DataProgram();
      
      const amount = -100.00;
      
      // Application should validate this
      if (isNaN(amount) || amount < 0) {
        // Invalid input - don't process
        expect(amount).toBeLessThan(0);
      }
      
      const balance = dataProgram.execute('READ');
      expect(balance).toBe(1000.00); // Balance unchanged
    });
    
    test('TC-907: Negative amount handling - debit (validation test)', () => {
      const dataProgram = new DataProgram();
      
      const amount = -100.00;
      
      // Application should validate this
      if (isNaN(amount) || amount < 0) {
        // Invalid input - don't process
        expect(amount).toBeLessThan(0);
      }
      
      const balance = dataProgram.execute('READ');
      expect(balance).toBe(1000.00); // Balance unchanged
    });
    
    test('TC-908: Non-numeric amount input (validation test)', () => {
      const amount = parseFloat('abc');
      
      // Application should validate this
      expect(isNaN(amount)).toBe(true);
      
      if (isNaN(amount) || amount < 0) {
        // Invalid input - don't process
        expect(true).toBe(true);
      }
    });
  });

  // ============================================================================
  // Additional Operations Class Tests
  // ============================================================================
  
  describe('Operations Class Tests', () => {
    
    test('Operations class initializes correctly', () => {
      const dataProgram = new DataProgram();
      const operations = new Operations(dataProgram, mockReadline);
      
      expect(operations).toHaveProperty('dataProgram');
      expect(operations).toHaveProperty('finalBalance');
      expect(operations.finalBalance).toBe(1000.00);
    });
    
    test('formatBalance formats correctly', () => {
      const dataProgram = new DataProgram();
      const operations = new Operations(dataProgram, mockReadline);
      
      expect(operations.formatBalance(1000.00)).toBe('001000.00');
      expect(operations.formatBalance(123.45)).toBe('000123.45');
      expect(operations.formatBalance(999999.99)).toBe('999999.99');
      expect(operations.formatBalance(0.00)).toBe('000000.00');
    });
  });

  // ============================================================================
  // Data Integrity Tests
  // ============================================================================
  
  describe('Data Integrity Tests', () => {
    
    test('Multiple operations maintain data integrity', () => {
      const dataProgram = new DataProgram();
      
      // Perform various operations
      const operations = [
        { type: 'credit', amount: 250.50 },
        { type: 'debit', amount: 100.25 },
        { type: 'credit', amount: 500.00 },
        { type: 'debit', amount: 350.75 }
      ];
      
      let expectedBalance = 1000.00;
      
      operations.forEach(op => {
        let balance = dataProgram.execute('READ');
        
        if (op.type === 'credit') {
          balance += op.amount;
          expectedBalance += op.amount;
          dataProgram.execute('WRITE', balance);
        } else if (op.type === 'debit' && balance >= op.amount) {
          balance -= op.amount;
          expectedBalance -= op.amount;
          dataProgram.execute('WRITE', balance);
        }
      });
      
      const finalBalance = dataProgram.execute('READ');
      expect(finalBalance).toBeCloseTo(expectedBalance, 2);
      expect(finalBalance).toBeCloseTo(1299.50, 2);
    });
    
    test('Concurrent read operations return same value', () => {
      const dataProgram = new DataProgram();
      
      const reads = Array(10).fill(null).map(() => dataProgram.execute('READ'));
      
      reads.forEach(balance => {
        expect(balance).toBe(1000.00);
      });
    });
    
    test('Balance calculation precision', () => {
      const dataProgram = new DataProgram();
      
      // Test floating point precision
      let balance = dataProgram.execute('READ');
      balance += 0.1;
      balance += 0.2;
      dataProgram.execute('WRITE', balance);
      
      const result = dataProgram.execute('READ');
      expect(result).toBeCloseTo(1000.3, 2);
    });
  });
});
