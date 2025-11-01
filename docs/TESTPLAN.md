# Account Management System - Test Plan

## Purpose
This test plan validates the business logic and implementation of the COBOL Account Management System. It will be used for:
1. Validation with business stakeholders
2. Creating unit and integration tests for the Node.js migration
3. Ensuring feature parity during the modernization process

## Test Environment
- **Application**: Account Management System (COBOL)
- **Initial Balance**: $1,000.00
- **Balance Format**: Up to $999,999.99 (6 digits, 2 decimal places)

---

## Test Cases

### 1. Application Initialization Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-001 | Verify application starts successfully | Application compiled | 1. Run `./accountsystem` | Application displays main menu with 4 options | | | |
| TC-002 | Verify initial balance is set correctly | Application just started | 1. Select option 1 (View Balance) | Display shows balance of $1,000.00 | | | Default balance validation |
| TC-003 | Verify menu displays all options | Application running | 1. Observe main menu | Menu shows: 1. View Balance, 2. Credit Account, 3. Debit Account, 4. Exit | | | |

---

### 2. View Balance Functionality Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-101 | View initial balance | Fresh application start | 1. Select option 1 | Display "Current balance: 1000.00" | | | |
| TC-102 | View balance after credit | Balance credited with $500.00 | 1. Select option 1 | Display shows updated balance of $1,500.00 | | | |
| TC-103 | View balance after debit | Balance debited by $200.00 | 1. Select option 1 | Display shows updated balance of $800.00 | | | |
| TC-104 | View balance multiple times | Any state | 1. Select option 1<br>2. Select option 1 again | Both displays show same balance (read-only operation) | | | Verify no side effects |

---

### 3. Credit Account (Deposit) Functionality Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-201 | Credit account with valid amount | Current balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 500.00 | Display "Amount credited. New balance: 1500.00" | | | |
| TC-202 | Credit account with small amount | Current balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 0.01 | New balance: $1,000.01 | | | Test precision |
| TC-203 | Credit account with large amount | Current balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 50000.00 | New balance: $51,000.00 | | | |
| TC-204 | Credit account with decimal amount | Current balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 123.45 | New balance: $1,123.45 | | | |
| TC-205 | Multiple consecutive credits | Current balance: $1,000.00 | 1. Select option 2, enter 100.00<br>2. Select option 2, enter 200.00<br>3. Select option 2, enter 300.00 | Final balance: $1,600.00 | | | Test cumulative credits |
| TC-206 | Credit with zero amount | Current balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 0.00 | Balance remains $1,000.00 | | | Edge case |
| TC-207 | Credit near maximum balance | Current balance: $999,000.00 | 1. Select option 2<br>2. Enter amount: 999.00 | New balance: $999,999.00 | | | Test upper limit |
| TC-208 | Credit exceeding maximum balance | Current balance: $999,000.00 | 1. Select option 2<br>2. Enter amount: 2000.00 | Overflow behavior or error | | | Boundary test |

---

### 4. Debit Account (Withdrawal) Functionality Tests - Sufficient Funds

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-301 | Debit account with valid amount | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 300.00 | Display "Amount debited. New balance: 700.00" | | | |
| TC-302 | Debit account with small amount | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 0.01 | New balance: $999.99 | | | Test precision |
| TC-303 | Debit entire balance | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1000.00 | New balance: $0.00 | | | Boundary test |
| TC-304 | Debit with decimal amount | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 123.45 | New balance: $876.55 | | | |
| TC-305 | Multiple consecutive debits | Current balance: $1,000.00 | 1. Select option 3, enter 100.00<br>2. Select option 3, enter 200.00<br>3. Select option 3, enter 300.00 | Final balance: $400.00 | | | Test cumulative debits |
| TC-306 | Debit with zero amount | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 0.00 | Balance remains $1,000.00 | | | Edge case |

---

### 5. Debit Account (Withdrawal) Functionality Tests - Insufficient Funds

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-401 | Debit exceeding balance | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1500.00 | Display "Insufficient funds for this debit."<br>Balance remains $1,000.00 | | | Core business rule |
| TC-402 | Debit slightly exceeding balance | Current balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1000.01 | Display "Insufficient funds for this debit."<br>Balance remains $1,000.00 | | | Precision boundary |
| TC-403 | Debit from zero balance | Current balance: $0.00 | 1. Select option 3<br>2. Enter amount: 0.01 | Display "Insufficient funds for this debit."<br>Balance remains $0.00 | | | Edge case |
| TC-404 | Verify balance unchanged after failed debit | Current balance: $500.00 | 1. Select option 3, enter 600.00 (fail)<br>2. Select option 1 (view balance) | Balance still shows $500.00 | | | Data integrity check |
| TC-405 | Debit exactly balance plus one cent | Current balance: $999.99 | 1. Select option 3<br>2. Enter amount: 1000.00 | Display "Insufficient funds for this debit."<br>Balance remains $999.99 | | | Precision test |

---

### 6. Mixed Transaction Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-501 | Credit followed by debit | Current balance: $1,000.00 | 1. Select option 2, enter 500.00<br>2. Select option 3, enter 300.00<br>3. Select option 1 | Final balance: $1,200.00 | | | Transaction sequence |
| TC-502 | Debit followed by credit | Current balance: $1,000.00 | 1. Select option 3, enter 400.00<br>2. Select option 2, enter 200.00<br>3. Select option 1 | Final balance: $800.00 | | | Transaction sequence |
| TC-503 | Failed debit followed by successful debit | Current balance: $1,000.00 | 1. Select option 3, enter 1500.00 (fail)<br>2. Select option 3, enter 500.00 (success)<br>3. Select option 1 | Final balance: $500.00 | | | Error recovery |
| TC-504 | Multiple operations sequence | Current balance: $1,000.00 | 1. Credit 500.00 (1500.00)<br>2. Debit 300.00 (1200.00)<br>3. Credit 100.00 (1300.00)<br>4. Debit 800.00 (500.00)<br>5. View balance | Final balance: $500.00 | | | Complex workflow |
| TC-505 | View balance between transactions | Current balance: $1,000.00 | 1. Credit 200.00<br>2. View balance (1200.00)<br>3. Debit 100.00<br>4. View balance (1100.00) | All balance displays are accurate | | | Read consistency |

---

### 7. Menu Navigation and User Input Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-601 | Select invalid menu option (0) | Application running | 1. Enter choice: 0 | Display "Invalid choice, please select 1-4."<br>Menu redisplays | | | Input validation |
| TC-602 | Select invalid menu option (5) | Application running | 1. Enter choice: 5 | Display "Invalid choice, please select 1-4."<br>Menu redisplays | | | Input validation |
| TC-603 | Select invalid menu option (letter) | Application running | 1. Enter choice: A | Display "Invalid choice, please select 1-4."<br>Menu redisplays | | | Input validation |
| TC-604 | Menu redisplays after each operation | Any state | 1. Select any option 1-3<br>2. Complete operation | Menu automatically redisplays | | | User experience |
| TC-605 | Cancel operation after viewing prompt | Credit/Debit prompt displayed | 1. Select option 2 or 3<br>2. Observe behavior at amount prompt | System awaits input | | | Document behavior |

---

### 8. Application Exit Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-701 | Exit application with option 4 | Application running | 1. Select option 4 | Display "Exiting the program. Goodbye!"<br>Application terminates | | | |
| TC-702 | Exit after viewing balance | Balance viewed | 1. Select option 1<br>2. Select option 4 | Application exits cleanly | | | |
| TC-703 | Exit after credit operation | Credit completed | 1. Select option 2, credit amount<br>2. Select option 4 | Application exits cleanly | | | |
| TC-704 | Exit after debit operation | Debit completed | 1. Select option 3, debit amount<br>2. Select option 4 | Application exits cleanly | | | |
| TC-705 | Exit after failed operation | Failed debit attempt | 1. Attempt debit > balance<br>2. Select option 4 | Application exits cleanly | | | |

---

### 9. Data Persistence and State Management Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-801 | Balance persists during session | Any balance state | 1. Perform credit/debit<br>2. Perform other operations<br>3. View balance | Balance reflects all transactions | | | State consistency |
| TC-802 | Balance resets on application restart | Modified balance (e.g., $1,500.00) | 1. Exit application<br>2. Restart application<br>3. View balance | Balance resets to $1,000.00 | | | In-memory storage |
| TC-803 | Read operation doesn't modify balance | Any balance | 1. View balance<br>2. View balance again<br>3. Perform transaction | Read operations have no side effects | | | Data integrity |
| TC-804 | Write operation updates storage | Balance: $1,000.00 | 1. Credit $500.00<br>2. Debit $200.00<br>3. View balance | Balance correctly shows $1,300.00 | | | Write verification |

---

### 10. Boundary and Edge Case Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-901 | Maximum balance value | Balance: $999,999.99 | 1. View balance | Display shows correct value | | | Upper boundary |
| TC-902 | Minimum balance value | Balance: $0.00 | 1. View balance | Display shows $0.00 | | | Lower boundary |
| TC-903 | Debit exact balance amount | Balance: $1,234.56 | 1. Select option 3<br>2. Enter 1234.56 | New balance: $0.00 | | | Exact match |
| TC-904 | Credit with maximum decimal precision | Any balance | 1. Select option 2<br>2. Enter amount: 123.99 | Amount accepted and processed correctly | | | Decimal precision |
| TC-905 | Debit with maximum decimal precision | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 123.99 | New balance: $876.01 | | | Decimal precision |
| TC-906 | Negative amount handling - credit | Any balance | 1. Select option 2<br>2. Enter amount: -100.00 | System behavior documented | | | Invalid input test |
| TC-907 | Negative amount handling - debit | Any balance | 1. Select option 3<br>2. Enter amount: -100.00 | System behavior documented | | | Invalid input test |
| TC-908 | Non-numeric amount input | Any balance | 1. Select option 2 or 3<br>2. Enter text instead of number | System behavior documented | | | Invalid input test |

---

## Test Execution Guidelines

### For Stakeholder Validation
1. Execute tests in sequence within each category
2. Document actual results in the "Actual Result" column
3. Mark status as Pass/Fail based on expected vs actual results
4. Add comments for any deviations or observations
5. Review failed tests with stakeholders for business rule clarification

### For Node.js Migration
1. Use this test plan as the basis for unit test specifications
2. Each test case should map to at least one automated test
3. Priority test categories for automation:
   - **High Priority**: TC-2xx (Credit), TC-3xx (Debit - Success), TC-4xx (Debit - Fail)
   - **Medium Priority**: TC-1xx (View), TC-5xx (Mixed), TC-8xx (State)
   - **Low Priority**: TC-6xx (Navigation), TC-7xx (Exit), TC-9xx (Boundaries)

### Test Data Requirements
- Initial balance: $1,000.00
- Test amounts: $0.01, $0.00, $100.00, $500.00, $1,000.00, $50,000.00, $999,999.99
- Invalid inputs: negative numbers, text, special characters
- Boundary values: exact balance, balance ± $0.01

---

## Success Criteria

### Functional Requirements
- ✅ All view balance operations return correct values
- ✅ All credit operations successfully increase balance
- ✅ All valid debit operations successfully decrease balance
- ✅ All insufficient fund scenarios properly reject transactions
- ✅ Balance state is maintained correctly across operations
- ✅ Menu navigation works as expected
- ✅ Application exits cleanly

### Non-Functional Requirements
- ✅ No data corruption or unexpected state changes
- ✅ Balance calculations are accurate to 2 decimal places
- ✅ Error messages are clear and user-friendly
- ✅ Application handles invalid inputs gracefully

---

## Known Limitations (Current COBOL Implementation)

1. **No Persistence**: Balance resets to $1,000.00 on application restart
2. **Single Account**: Only one account supported (no account identification)
3. **No Transaction History**: No audit trail or transaction log
4. **Limited Validation**: Minimal input validation for amount fields
5. **No Authentication**: No user authentication or authorization
6. **In-Memory Storage**: Balance stored in memory, not in database
7. **No Concurrent Access**: Not designed for multiple simultaneous users
8. **Format Constraints**: Balance limited to 6 digits + 2 decimals ($999,999.99 max)

---

## Recommendations for Node.js Implementation

### Must Have
1. Implement all test cases as automated unit and integration tests
2. Add input validation for amount fields (positive numbers, proper format)
3. Implement proper error handling and logging
4. Add persistence layer (database or file storage)
5. Maintain identical business logic for feature parity

### Should Have
1. Add transaction history/audit trail
2. Support multiple accounts with unique identifiers
3. Implement authentication and authorization
4. Add API endpoints for programmatic access
5. Improve error messages and user feedback

### Nice to Have
1. Add transaction limits and account limits
2. Support different transaction types
3. Add date/time stamps to all operations
4. Implement concurrent access controls
5. Add reporting and analytics capabilities

---

## Sign-off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Stakeholder | | | |
| QA Lead | | | |
| Development Lead | | | |
| Project Manager | | | |

---

**Document Version**: 1.0  
**Last Updated**: November 1, 2025  
**Next Review Date**: TBD
