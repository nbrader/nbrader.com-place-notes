# PlaceNotes - High-Level Requirements Specification

## Core Purpose
A spatial note-taking application that allows users to organize text notes freely in 2D space.

---

## Functional Requirements

### 1. Note Creation & Management
- User can create text notes by typing text and placing them on a canvas
- Each note displays its text content visually on the canvas
- Notes must be distinguishable from each other and the background
- Notes must be visible and interactive regardless of text length (including empty text)
- System must prevent creation of invisible or unusable notes

### 2. Note Editing
- User can select any existing note to edit its text
- Clicking/tapping a note automatically populates the input field with that note's text
- Selected notes must have clear visual indication (e.g., different border, highlight)
- Text changes must update the note's display immediately (real-time sync)
- The note's size should adapt to accommodate its text content
- Editing creates a two-way binding: input field ↔ selected note

### 3. Spatial Navigation
- User can navigate an unbounded 2D workspace (infinite canvas)
- User can pan/scroll the viewport to explore different areas
- All notes maintain their absolute positions in the workspace
- The workspace has no boundaries or size limits
- Camera/viewport movement must be smooth and responsive

### 4. Note Positioning
- User can drag notes to reposition them anywhere in the workspace
- Notes can be placed at any coordinate in the 2D space
- Dragging should provide smooth, responsive visual feedback
- Live preview of note position during drag operations
- Final position committed when drag ends

### 5. Note Deletion
- User can delete notes they no longer need
- Deletion must be intentional (not accidental during other operations)
- System should provide a way to switch between editing/moving and deletion modes
- Mode switching must be explicit and clear to the user

### 6. Visual Hierarchy
- When notes overlap, newer/recently created notes should appear on top
- User should be able to interact with the topmost note when multiple notes overlap
- The visual stacking order must be consistent and predictable
- Z-order (depth ordering) must match user expectations

### 7. State Persistence
- User can export the entire workspace state (all notes and their positions)
- User can import a previously exported state to restore their work
- The export format should be human-readable and editable (JSON)
- Users should be able to manually edit the exported data and re-import it
- Import/export must be lossless (no data corruption)
- Invalid data during import should not crash the application
- Real-time synchronization between visual state and export representation

### 8. Interaction Modes
The application must support at least two distinct modes:

#### Edit/Move Mode
- Create new notes by clicking/tapping location
- Select and edit existing notes
- Drag notes to reposition them
- Pan/scroll the canvas by dragging background

#### Delete Mode
- Click/tap notes to remove them
- Cannot accidentally delete while editing or moving

**Mode Requirements:**
- User must be able to switch between modes easily
- Current mode should be clearly indicated in the UI
- Mode behavior must be mutually exclusive and well-defined

---

## Non-Functional Requirements

### Performance
- The application must remain responsive with hundreds of notes
- Dragging operations must be smooth with no visible lag (target: 60fps)
- Rendering must be efficient enough for real-time updates
- List operations should be optimized (avoid O(n²) algorithms)
- No unnecessary re-computation during interactive operations

### Usability
- Single-click/tap interaction to select and begin editing a note
- Intuitive drag-and-drop for repositioning
- Clear visual feedback for all interactions
- No accidental data loss from UI interactions
- Touch-friendly UI elements (adequate size and spacing)
- Responsive design that works across device sizes

### Platform
- Must work in web browsers on desktop and mobile devices
- Must support both mouse/pointer input (desktop) and touch input (mobile/tablets)
- Touch interactions should map to equivalent mouse interactions:
  - Tap = Click
  - Touch and drag = Click and drag
  - Touch lift = Mouse up
- Should prevent default touch behaviors that interfere with app functionality (e.g., page scrolling during drag)
- Should work on standard screen sizes including phone screens
- UI elements (buttons, input fields) must be usable on touch devices
- Cross-browser compatibility (modern browsers)

### Data Integrity
- Notes must maintain their content and position accurately
- State export/import must be lossless
- Invalid data during import should not crash the application
- JSON parsing errors should be handled gracefully
- User should be able to recover from invalid states

---

## User Workflows

### Workflow 1: Create and Organize Notes
1. Type text in input field
2. Click/tap "Add" button or designated location
3. Note appears at location
4. Repeat to create multiple notes
5. Pan viewport to explore and organize notes spatially
6. Drag notes to adjust positions and create spatial relationships

### Workflow 2: Edit Existing Note
1. Click/tap on an existing note
2. Note is selected (visual indicator appears)
3. Input field automatically populates with note's text
4. Edit text in input field
5. See changes reflected immediately on the note
6. Note width adjusts automatically to fit new text
7. Click another note to edit that one instead

### Workflow 3: Reorganize Layout
1. Switch to move mode (if needed)
2. Drag notes to new positions
3. Pan viewport to access different areas
4. Notes maintain their spatial relationships
5. Organize notes into meaningful clusters or layouts

### Workflow 4: Clean Up Workspace
1. Switch to deletion mode
2. Click/tap notes to remove them
3. Confirm removals are intentional (via mode switch)
4. Switch back to edit mode to continue working

### Workflow 5: Save and Restore Work
1. View JSON representation in textarea (auto-updates)
2. Copy JSON to external storage (file, clipboard, etc.)
3. Later, paste JSON back into textarea
4. Application automatically restores all notes and positions
5. Can manually edit JSON for bulk changes if needed

### Workflow 6: Mobile Usage
1. Tap note to select and edit
2. Touch and drag note to move it
3. Touch and drag background to pan camera
4. Pinch or spread (future enhancement for zoom)
5. Use on-screen keyboard for text input
6. All functionality available through touch

---

## Technical Constraints

### Data Model
- Notes must have unique identifiers (never reused)
- Notes have position (x, y coordinates in world space)
- Notes have size (width, height in pixels)
- Notes have text content (string)
- World coordinates are independent of screen/viewport coordinates
- Camera/viewport has position (offset from world origin)

### State Management
- Application state must be serializable to JSON
- Transient UI state (dragging, selection) separate from persistent state
- Two-way binding between textarea and application state
- Validation of imported state before applying

### Rendering
- Notes rendered in correct z-order (newest on top)
- Efficient rendering (avoid re-rendering unchanged elements)
- Coordinate transformation: world → screen (camera offset)
- Visual feedback during interactions (selection, dragging)

---

## Future Enhancements (Out of Scope)

While these are not current requirements, they represent potential future directions:

- Zoom in/out functionality (pinch on mobile, scroll on desktop)
- Note colors and styling options
- Rich text formatting within notes
- Note resizing (manual width/height adjustment)
- Undo/redo functionality
- Multi-select and bulk operations
- Search/filter notes by content
- Export to different formats (PDF, image, etc.)
- Collaborative editing (multi-user)
- Note linking and relationships
- Tags and categories
- Attachments (images, files)
- Grid snapping for alignment
- Keyboard shortcuts for power users

---

## Success Criteria

The application is considered successful if:

1. ✅ Users can create, edit, move, and delete notes
2. ✅ The spatial organization is intuitive and persistent
3. ✅ Performance remains smooth with hundreds of notes
4. ✅ Works seamlessly on both desktop and mobile devices
5. ✅ Data can be saved and restored without loss
6. ✅ Users can accomplish all workflows without confusion
7. ✅ No crashes or data corruption under normal usage
8. ✅ Visual feedback makes interactions clear and predictable

---

## Implementation-Agnostic Design

These requirements are intentionally technology-agnostic. They describe **what** the application must do, not **how** to implement it. Valid implementations could use:

- Any programming language (Elm, JavaScript, TypeScript, Python, etc.)
- Any framework (React, Vue, Angular, Svelte, vanilla, etc.)
- Any rendering approach (HTML/CSS, Canvas, SVG, WebGL, etc.)
- Any state management pattern (Elm Architecture, Redux, MobX, etc.)
- Any storage mechanism (localStorage, IndexedDB, server, etc.)

The key is that the implementation satisfies all functional and non-functional requirements listed above.
