# TodoistSDK

[![Hackage](https://img.shields.io/hackage/v/todoist-sdk.svg)](https://hackage.haskell.org/package/todoist-sdk)

An unofficial Haskell SDK for the [Todoist REST API](https://developer.todoist.com/rest/v2/). Manage projects, tasks, comments, sections, and labels with type-safe, ergonomic Haskell functions.

## Features

- **Complete API Coverage**: Projects, Tasks, Comments, Sections, and Labels
- **Type-Safe**: Leverages Haskell's type system to prevent common errors
- **Ergonomic Builder Pattern**: Easily construct API requests with optional fields
- **Automatic Pagination**: Transparently fetches all pages for list operations
- **Flexible Error Handling**: Operations return `Either TodoistError a` for explicit error handling
- **Testing Support (In Progress)**: Includes Trace interpreter for testing without API calls

## Installation

### Using Stack

Add to your `stack.yaml`:

```yaml
extra-deps:
  - todoist-sdk-0.1.1.1
```

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - todoist-sdk
```

### Using Cabal

Add to your `.cabal` file's `build-depends`:

```cabal
build-depends:
    base >=4.7 && <5
  , todoist-sdk
```

Then install:

```bash
cabal update
cabal install todoist-sdk
```

## Quick Start

Get your API token from [Todoist Settings → Integrations → Developer](https://todoist.com/prefs/integrations).

### Try it in the REPL

```bash
$ stack repl
```

```haskell
>>> import Web.Todoist
>>> let config = newTodoistConfig "your-api-token-here"
>>> result <- todoist config getAllProjects
>>> case result of
      Left err -> print err
      Right projects -> mapM_ print projects
```

### Complete Example

Create a file `example.hs`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-24.7 script --package todoist-sdk --package text

{-# LANGUAGE OverloadedStrings #-}

import Web.Todoist

main :: IO ()
main = do
    -- Configure with your API token
    let config = newTodoistConfig "your-api-token-here"

    result <- todoist config $ do
        -- Create a new project
        let newProj = runBuilder (newProject "My Haskell Project")
                      (withDescription "Learning Haskell SDK" <> withViewStyle Board)
        project <- addProject newProj

        -- Create a task in the project
        let newTask = runBuilder (newTask "Read documentation")
                      (withProjectId (_id project) <> withPriority 2)
        task <- addTask newTask

        -- Get all tasks
        tasks <- getTasks emptyTaskParam

        pure (project, task, tasks)

    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (proj, task, tasks) -> do
            putStrLn $ "Created project: " ++ show (_name proj)
            putStrLn $ "Created task: " ++ show (_content task)
            putStrLn $ "Total tasks: " ++ show (length tasks)
```

Run it:

```bash
chmod +x example.hs
./example.hs
```

## Common Usage Examples

### Working with Projects

```haskell
import Web.Todoist

let config = newTodoistConfig "your-api-token"

-- Get all projects
result <- todoist config getAllProjects

-- Create a project with optional fields
let newProject = runBuilder (newProject "Shopping List")
                 (withColor "blue" <> withViewStyle List <> withIsFavorite True)
project <- todoist config (addProject newProject)

-- Update a project
let update = runBuilder emptyProjectUpdate (withName "Updated Name")
updated <- todoist config (updateProject update projectId)

-- Delete a project
todoist config (deleteProject projectId)
```

### Working with Tasks

```haskell
-- Create a task with due date
let task = runBuilder (newTask "Buy milk")
           (withProjectId "project-123"
            <> withDueString "tomorrow"
            <> withPriority 3
            <> withLabels ["grocery", "urgent"])
result <- todoist config (addTask task)

-- Get tasks with filters
let params = TaskParam
    { project_id = Just "project-123"
    , filter = Nothing
    , label_id = Nothing
    , cursor = Nothing
    , limit = Nothing
    }
tasks <- todoist config (getTasks params)

-- Complete a task
todoist config (closeTask taskId)

-- Update a task
let update = runBuilder emptyTaskPatch (withContent "Buy 2% milk")
updated <- todoist config (updateTask update taskId)
```

### Working with Comments

```haskell
-- Add a comment to a task
let comment = runBuilder (newComment "Don't forget organic!")
              (withTaskId "task-456")
result <- todoist config (addComment comment)

-- Get all comments for a project
let params = CommentParam
    { project_id = Just "project-123"
    , task_id = Nothing
    , cursor = Nothing
    , limit = Nothing
    , public_key = Nothing
    }
comments <- todoist config (getComments params)
```

### Working with Sections

```haskell
-- Create a section
let section = runBuilder (newSection "In Progress" "project-123") mempty
result <- todoist config (addSection section)

-- Get sections for a project with builder pattern
let params = runBuilder newSectionParam (withProjectId "project-123" <> withLimit 50)
sections <- todoist config (getSections params)
```

### Working with Labels

```haskell
-- Create a label
let label = runBuilder (newLabel "urgent") mempty
result <- todoist config (addLabel label)

-- Get all labels
let params = runBuilder newLabelParam (withLimit 50)
labels <- todoist config (getLabels params)
```

## Error Handling

All operations return `Either TodoistError a`. The `TodoistError` type includes:

- `BadRequest` - Invalid request parameters
- `Unauthorized` - Invalid or missing API token
- `Forbidden` - Insufficient permissions
- `NotFound` - Resource doesn't exist
- `HttpError String` - Other HTTP errors

Example:

```haskell
result <- todoist config (getProject projectId)
case result of
    Left BadRequest -> putStrLn "Invalid project ID"
    Left Unauthorized -> putStrLn "Check your API token"
    Left NotFound -> putStrLn "Project not found"
    Left (HttpError msg) -> putStrLn $ "HTTP error: " ++ msg
    Right project -> print project
```

## Documentation

Full API documentation is available on [Hackage](https://hackage.haskell.org/package/todoist-sdk).

For details on the Todoist REST API, see the [official documentation](https://developer.todoist.com/rest/v2/).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Acknowledgments

This library is a labor of love to the Haskell community and to the Todoist app. It is an unofficial SDK and is not affiliated with or endorsed by Doist.
