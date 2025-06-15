# Imperative vs Declarative: Go vs Erlang

## Core Mental Models

### **Go: Imperative ("How to do it")**
> **"Do this, then that, then this, then you're finished"**

Step-by-step instructions telling the computer exactly how to accomplish a task.

### **Erlang: Declarative ("What should happen")**
> **"If you get this input, do that; otherwise, do this"**

Describe what the result should be based on the input, letting pattern matching handle the flow.

---

## 1. Function Definition Approaches

### **Go: Single Function Body (Imperative)**
```go
func greet(gender string, name string) string {
    if gender == "male" {
        return fmt.Sprintf("Hello, Mr. %s!", name)
    } else if gender == "female" {
        return fmt.Sprintf("Hello, Mrs. %s!", name)
    } else {
        return fmt.Sprintf("Hello, %s!", name)
    }
}
```

**Mental Model**: "Check if gender is male, then do this. Otherwise check if female, then do that. Otherwise do something else."

### **Erlang: Multiple Function Clauses (Declarative)**
```erlang
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).
```

**Mental Model**: "When the input matches male and Name, produce this result. When it matches female and Name, produce that result. For anything else with Name, produce this other result."

---

## 2. Finding an Element (The Classic Example)

### **Go: Imperative Loop**
```go
func findElement(slice []int, target int) bool {
    // HOW: Step through each element
    for i := 0; i < len(slice); i++ {
        if slice[i] == target {
            return true  // Found it, stop here
        }
    }
    return false  // Went through all, didn't find it
}
```

**Imperative thinking**:
1. Start at the beginning
2. Check current element
3. If found, return true
4. Move to next element
5. Repeat until end
6. Return false if not found

### **Erlang: Declarative Pattern Matching**
```erlang
% WHAT: Define what each case means
find_element([], _Target) -> false;                    % Empty list means not found
find_element([Target|_], Target) -> true;              % First element matches = found
find_element([_|Tail], Target) -> find_element(Tail, Target).  % Skip first, check rest
```

**Declarative thinking**:
- "If the list is empty, the answer is false"
- "If the first element is the target, the answer is true"
- "If the first element isn't the target, the answer is whatever find_element returns for the rest"

---

## 3. List Processing Patterns

### **Go: Explicit Steps**
```go
// Calculate sum of squares
func sumSquares(numbers []int) int {
    sum := 0
    for _, num := range numbers {
        square := num * num
        sum += square
    }
    return sum
}

// Filter positive numbers
func filterPositive(numbers []int) []int {
    var result []int
    for _, num := range numbers {
        if num > 0 {
            result = append(result, num)
        }
    }
    return result
}
```

### **Erlang: Describe the Transformation**
```erlang
% Calculate sum of squares - describe what we want
sum_squares(List) ->
    Squares = [X*X || X <- List],          % "Square each element"
    lists:sum(Squares).                    % "Sum the results"

% Filter positive - describe the condition
filter_positive(List) ->
    [X || X <- List, X > 0].              % "Keep elements where X > 0"

% Or with pattern matching function clauses
sum_squares([]) -> 0;                                    % Empty list sums to 0
sum_squares([H|T]) -> H*H + sum_squares(T).            % Head squared plus sum of tail
```

---

## 4. Control Flow Philosophy

### **Go: Conditional Branching (Tell how to decide)**
```go
func processAccount(account Account) string {
    if account.Balance < 0 {
        return "Overdrawn account"
    } else if account.Balance == 0 {
        return "Zero balance"
    } else if account.Balance < 1000 {
        return "Low balance"
    } else {
        return "Sufficient funds"
    }
}
```

### **Erlang: Pattern-Based Classification (Describe what each case is)**
```erlang
process_account({account, Balance}) when Balance < 0 ->
    "Overdrawn account";
process_account({account, 0}) ->
    "Zero balance";
process_account({account, Balance}) when Balance < 1000 ->
    "Low balance";
process_account({account, _}) ->
    "Sufficient funds".
```

**Key Difference**:
- **Go**: "Check the balance and decide what to do"
- **Erlang**: "Define what each type of account means"

---

## 5. Error Handling Approaches

### **Go: Explicit Error Checking (Imperative)**
```go
func processFile(filename string) error {
    // Step 1: Try to open file
    file, err := os.Open(filename)
    if err != nil {
        return fmt.Errorf("failed to open: %w", err)
    }
    defer file.Close()

    // Step 2: Try to read content
    content, err := io.ReadAll(file)
    if err != nil {
        return fmt.Errorf("failed to read: %w", err)
    }

    // Step 3: Try to process content
    err = processContent(content)
    if err != nil {
        return fmt.Errorf("failed to process: %w", err)
    }

    return nil
}
```

### **Erlang: Pattern Match on Results (Declarative)**
```erlang
process_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            process_content(Content);
        {error, Reason} ->
            {error, {file_read_failed, Reason}}
    end.

% Or even more declarative with function clauses
handle_file_result({ok, Content}) ->
    process_content(Content);
handle_file_result({error, Reason}) ->
    {error, {file_read_failed, Reason}}.
```

---

## 6. The Fundamental Difference in Thinking

### **Imperative (Go) - Recipe Approach**
Like giving cooking instructions:
1. "First, heat the pan"
2. "Then, add oil"
3. "Next, add onions"
4. "Stir for 2 minutes"
5. "If onions are golden, add garlic"

### **Declarative (Erlang) - Definition Approach**
Like defining what things are:
- "A properly cooked onion is golden and translucent"
- "A dish is ready when all ingredients are properly cooked"
- "If ingredients aren't ready, continue cooking them"

---

## 7. Real-World Example: Factorial

### **Go: Step-by-Step Instructions**
```go
func factorial(n int) int {
    result := 1
    for i := 1; i <= n; i++ {
        result *= i
    }
    return result
}
```

**Imperative**: "Start with 1, then multiply by each number from 1 to n"

### **Erlang: Mathematical Definition**
```erlang
factorial(0) -> 1;                           % By definition, 0! = 1
factorial(N) -> N * factorial(N-1).          % By definition, N! = N × (N-1)!
```

**Declarative**: "Factorial of 0 is 1. Factorial of N is N times factorial of N-1."

---

## 8. The "What vs How" Summary

| Aspect | Go (Imperative) | Erlang (Declarative) |
|--------|----------------|----------------------|
| **Focus** | HOW to compute | WHAT to compute |
| **Style** | Step-by-step instructions | Mathematical definitions |
| **Control** | Explicit loops and conditions | Pattern matching clauses |
| **Errors** | Check each step | Pattern match results |
| **Flow** | Sequential execution | Rule-based transformation |
| **Mindset** | "Do this, then this, then this" | "If this, then that" |

---

## Key Insight from the Project Knowledge

> **"A different aspect of recursive definitions when compared to their imperative counterparts (usually in while or for loops) is that instead of taking a step-by-step approach ('do this, then that, then this, then you're finished'), our approach is more declarative ('if you get this input, do that; otherwise, do this'). This property is made more obvious with the help of pattern matching in function heads."**

The fundamental shift is from **procedural instructions** (Go) to **mathematical relationships** (Erlang). Erlang's approach mirrors how you might define mathematical functions - not by describing the algorithm to compute them, but by stating what they equal under different conditions.
