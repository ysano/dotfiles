# TMux Claude Voice - Architecture Decision Records (ADRs)

## ADR Template

```markdown
# ADR-XXXX: [Short Title]

## Status
[Proposed | Accepted | Deprecated | Superseded]

## Context
[Describe the forces at play, including technological, political, social, and project local]

## Decision
[State the architecture decision and explain why]

## Consequences
[Describe the resulting context, both positive and negative]

## Alternatives Considered
[List other options that were evaluated]

## References
[Links to supporting documentation, discussions, or external resources]
```

---

## ADR-0001: Adopt Shell Script as Primary Implementation Language

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Architecture Team

### Context
The TMux Claude Voice system needed a implementation language that would:
- Integrate seamlessly with tmux (which expects shell scripts for hooks)
- Provide cross-platform compatibility (WSL, macOS, Linux)
- Allow direct interaction with OS-level audio systems
- Minimize external dependencies
- Enable rapid prototyping and iteration

Alternative languages considered: Python, Node.js, Go, Rust

### Decision
We will use **Bash shell scripts (4.0+)** as the primary implementation language for the TMux Claude Voice system.

### Consequences

**Positive**:
- Native tmux integration through hooks without additional processes
- Direct access to platform-specific commands (powershell.exe, osascript, espeak)
- No runtime dependencies beyond standard POSIX tools
- Excellent portability across Unix-like systems
- Fast startup time and low memory footprint
- Easy debugging and system integration

**Negative**:
- Limited built-in data structures (arrays, associative arrays only in Bash 4+)
- String processing capabilities less robust than Python/Node.js
- Error handling requires careful design patterns
- Complex logic can become difficult to maintain
- Limited library ecosystem compared to modern languages

**Mitigation Strategies**:
- Establish strict coding standards and naming conventions
- Implement comprehensive error handling patterns
- Use external tools (jq, yq) for complex data processing
- Maintain extensive test coverage
- Document all non-obvious shell patterns

### Alternatives Considered

1. **Python**
   - Pros: Rich libraries, excellent string processing, robust error handling
   - Cons: Additional dependency, slower startup, tmux integration complexity

2. **Node.js**
   - Pros: Excellent async I/O, JSON processing, large ecosystem
   - Cons: Heavy runtime, complex installation, overkill for system integration

3. **Go**
   - Pros: Fast execution, cross-compilation, static binaries
   - Cons: Compilation step, less suitable for configuration-driven behavior

---

## ADR-0002: Platform Abstraction Layer Architecture

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Core Development Team

### Context
The system needs to support multiple platforms (WSL, macOS, Linux) with different:
- Audio systems (PowerShell+TTS, say+Audio Units, espeak+ALSA/PulseAudio)
- File system layouts (/mnt/c, /Users, /home)
- Command availability and behavior
- Permission models

We needed an architecture that would allow platform-specific implementations while maintaining a unified API.

### Decision
Implement a **Platform Abstraction Layer** using the Adapter Pattern:
- Core unified API in `universal_voice.sh`
- Platform detection in `platform_utils.sh`
- Platform-specific adapters in `platforms/{wsl,macos,linux}/`
- Each platform implements standardized interface functions

### Consequences

**Positive**:
- Clean separation of platform-specific code
- New platform support requires only implementing the standard interface
- Core logic remains platform-agnostic
- Testing can be done per-platform independently
- Easy to add platform-specific optimizations

**Negative**:
- Additional abstraction layer complexity
- Some platform-specific features may not translate well to unified API
- Requires discipline to avoid leaking platform details into core logic
- More files and directories to maintain

**Architecture Pattern**:
```bash
# Core API
universal_speak(text, voice, speed, volume)

# Platform Detection  
platform=$(detect_current_platform)

# Platform Routing
case "$platform" in
    "wsl") wsl_speak_text "$text" "$voice" "$speed" "$volume" ;;
    "macos") macos_speak_text "$text" "$voice" "$speed" "$volume" ;;
    "linux") linux_speak_text "$text" "$voice" "$speed" "$volume" ;;
esac
```

### Alternatives Considered

1. **Monolithic Script with Conditionals**
   - Pros: Simpler file structure
   - Cons: Code becomes unmaintainable as platforms grow

2. **Plugin Architecture**
   - Pros: Maximum flexibility
   - Cons: Over-engineering for current scope

3. **Compilation-Time Platform Selection**
   - Pros: Single binary per platform
   - Cons: Distribution complexity, runtime platform detection needed anyway

---

## ADR-0003: YAML-Based Unified Configuration System

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Configuration Team

### Context
The system initially had:
- 21 different configuration files in various formats
- `.conf` files (tmux format)
- Shell variable files
- Environment variable definitions
- 407 scattered environment variable references

This created maintenance burden, inconsistency, and difficulty in managing cross-platform configurations.

### Decision
Implement a **Unified YAML Configuration System**:
- Single source of truth: `tmux-unified.yaml`
- Hierarchical configuration with inheritance
- Automatic generation of platform-specific configurations
- Schema validation and migration support

### Consequences

**Positive**:
- 95% reduction in configuration files (21 → 1)
- Consistent configuration format across all components
- Version control friendly (YAML diffs are readable)
- Schema validation prevents configuration errors
- Easy to add new configuration options
- Platform-specific overrides supported
- Automated migration from legacy configurations

**Negative**:
- Requires YAML parsing tools (yq or Python)
- Additional generation step before deployment
- Learning curve for users familiar with direct tmux configuration
- Potential single point of failure

**Implementation Pattern**:
```yaml
# Unified Configuration Structure
base:
  shell: {default_command: "zsh"}
  prefix: {key: "C-z"}

platforms:
  wsl:
    clipboard: {enabled: true, command: "clip.exe"}
  macos:
    audio: {unit_integration: true}

claude_voice:
  states:
    complete: {enabled: true, voice_synthesis: true}
```

### Alternatives Considered

1. **Keep Distributed Configuration**
   - Pros: No migration needed
   - Cons: Continued maintenance burden and inconsistency

2. **JSON Configuration**
   - Pros: Ubiquitous tooling support
   - Cons: Less human-readable, no comments support

3. **TOML Configuration**
   - Pros: Good for complex hierarchies
   - Cons: Less familiar to users, limited tooling in shell environment

---

## ADR-0004: Equal Power Pan Law for Audio Positioning

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Audio Engineering Team

### Context
The system provides audio positioning capabilities for status-specific sound effects. We needed to choose an audio panning algorithm that would:
- Provide natural stereo positioning
- Maintain consistent perceived volume across the stereo field
- Be computationally efficient in shell environment
- Sound professional and non-distracting

### Decision
Implement **Equal Power Pan Law with 3dB center attenuation**:
- Uses trigonometric functions: `cos(pan_angle)` and `sin(pan_angle)`
- Center position is 3dB lower than extremes
- Pan range: -1.0 (full left) to +1.0 (full right)

### Consequences

**Positive**:
- Maintains constant perceived power across stereo field
- Industry-standard algorithm used in professional audio
- Smooth transitions without dead spots
- Mathematical precision ensures repeatability

**Negative**:
- Requires floating-point arithmetic (bc command)
- More complex than linear panning
- 3dB center reduction may be unexpected to some users

**Implementation**:
```bash
apply_equal_power_pan() {
    local pan_position="$1"    # -1.0 to 1.0
    local pan_angle=$(echo "$pan_position * 0.785398" | bc -l)  # π/4
    local left_gain=$(echo "c($pan_angle) * 0.707107" | bc -l)   # cos * √2/2
    local right_gain=$(echo "s($pan_angle) * 0.707107" | bc -l)  # sin * √2/2
}
```

### Alternatives Considered

1. **Linear Panning**
   - Pros: Simple implementation
   - Cons: Perceived volume changes across stereo field

2. **Constant Power (6dB center)**
   - Pros: Maintains constant RMS power
   - Cons: Center sounds too loud, less natural

3. **No Panning Support**
   - Pros: Simplest implementation
   - Cons: Missed opportunity for enhanced user experience

---

## ADR-0005: File-Based Caching with TTL

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Performance Team

### Context
The system performs several expensive operations that could benefit from caching:
- Platform detection (subprocess execution)
- Voice synthesis results (network/CPU intensive)
- Configuration parsing (file I/O intensive)
- AI API responses (network latency)

We needed a caching solution that would work in shell environment without external dependencies.

### Decision
Implement **File-Based Caching with Time-To-Live (TTL)**:
- Cache keys are SHA-256 hashes of input parameters
- Cache files stored in `/tmp/tmux-claude-cache/`
- TTL implemented using file modification time
- Multi-level caching: memory (shell variables) → disk → source

### Consequences

**Positive**:
- No external dependencies (uses standard file system)
- TTL automatically handled by file timestamps
- Survives process restarts (persistent cache)
- Easy to implement cache invalidation (delete files)
- Cache size is self-limiting (tmp filesystem)

**Negative**:
- File system overhead for small cache entries
- No automatic cache size management
- Potential race conditions in concurrent access
- Manual cleanup required for disk space management

**Implementation Pattern**:
```bash
cache_get() {
    local key="$1"
    local cache_file="$cache_dir/$(echo "$key" | sha256sum | cut -d' ' -f1)"
    
    [[ -f "$cache_file" ]] || return 1
    
    # Check TTL
    local file_age=$(($(date +%s) - $(stat -c %Y "$cache_file")))
    [[ $file_age -lt $cache_ttl ]] || { rm -f "$cache_file"; return 1; }
    
    cat "$cache_file"
}
```

### Alternatives Considered

1. **Redis/Memcached**
   - Pros: High performance, built-in TTL, atomic operations
   - Cons: External dependency, overkill for simple use case

2. **SQLite Database**
   - Pros: ACID properties, complex queries, built-in TTL
   - Cons: Requires sqlite3 command, more complexity

3. **Memory-Only Caching**
   - Pros: Fastest access
   - Cons: Lost on process restart, limited by shell variable constraints

---

## ADR-0006: PowerShell Interop for WSL Voice Synthesis

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: WSL Integration Team

### Context
WSL (Windows Subsystem for Linux) presents unique challenges:
- Linux environment but Windows host system
- Need access to Windows TTS engines for high-quality voice synthesis
- Security boundaries between WSL and Windows
- Different execution contexts and permissions

Native Linux TTS solutions (espeak, festival) provide lower quality compared to Windows Speech Platform.

### Decision
Use **PowerShell interop** to access Windows Speech Platform from WSL:
- Execute `powershell.exe` from WSL with speech synthesis commands
- Use .NET System.Speech.Synthesis namespace
- Implement timeout and error handling for cross-boundary calls
- Cache PowerShell availability to avoid repeated checks

### Consequences

**Positive**:
- Access to high-quality Windows TTS engines (Haruka, Sayaka, etc.)
- Leverages existing Windows speech infrastructure
- No additional Windows software installation required
- Japanese language support with native Windows voices

**Negative**:
- Dependency on PowerShell execution policy
- Cross-platform boundary introduces latency
- Complex error handling across execution contexts
- Security implications of executing Windows commands from Linux

**Implementation**:
```bash
wsl_speak_text() {
    local text="$1"
    local voice="${2:-Microsoft Haruka Desktop}"
    local escaped_text=$(escape_powershell_string "$text")
    
    local ps_command="Add-Type -AssemblyName System.Speech; \
\$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer; \
\$synth.SelectVoice('$voice'); \
\$synth.Speak('$escaped_text');"
    
    timeout 30s powershell.exe -Command "$ps_command"
}
```

### Alternatives Considered

1. **Native Linux TTS Only**
   - Pros: No cross-platform complexity
   - Cons: Lower quality speech synthesis, limited Japanese support

2. **Windows Subsystem for Linux Audio (WSLg)**
   - Pros: More native audio integration
   - Cons: Requires Windows 11, limited availability

3. **Network-Based TTS Service**
   - Pros: Consistent across platforms
   - Cons: Network dependency, privacy concerns, API costs

---

## ADR-0007: Three-State Claude Code Status Detection

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: User Experience Team

### Context
Claude Code (the AI assistant) exhibits different states during interaction:
- Processing user requests (busy)
- Waiting for user input (idle/waiting)
- Completed tasks (finished/success)

Users needed audio feedback that matched these states to understand system status without looking at screen.

### Decision
Implement **Three-State Status Detection** with distinct audio signatures:
- `⚡ Busy`: Warning pattern (800Hz×2→600Hz) - short, attention-getting
- `⌛ Waiting`: Ascending melody (659Hz→880Hz→1175Hz) - gentle, non-intrusive
- `✅ Complete`: Success pattern (523Hz→659Hz→783Hz→1046Hz) - satisfying, conclusive

### Consequences

**Positive**:
- Clear audio differentiation between system states
- Frequencies chosen to be pleasant and non-jarring
- Pattern lengths optimized for quick recognition
- Supports workflow awareness without visual attention

**Negative**:
- Limited to three states (may need expansion)
- Audio patterns require user learning curve
- May be distracting in quiet environments
- Cultural differences in audio perception

**Sound Pattern Design**:
```bash
declare -A STATUS_SOUND_CONFIGS=(
    ["⚡"]="frequency:800,800,600|duration:80,80,100"
    ["⌛"]="frequency:659,880,1175|duration:100,150,100" 
    ["✅"]="frequency:523,659,783,1046|duration:80,80,80,120"
)
```

### Alternatives Considered

1. **Binary Status (Working/Idle)**
   - Pros: Simpler implementation
   - Cons: Less granular feedback, misses completion events

2. **Five-State System**
   - Pros: More granular status information
   - Cons: Too complex for audio differentiation, cognitive overload

3. **Speech-Only Notifications**
   - Pros: Clear semantic meaning
   - Cons: Slower delivery, language dependency, more intrusive

---

## ADR-0008: Session-Isolated State Management

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Architecture Team

### Context
TMux supports multiple concurrent sessions, each potentially with different:
- User preferences for voice settings
- Work contexts (different projects, languages, noise tolerance)
- Performance characteristics
- Error states and recovery needs

A global state approach would create conflicts and reduce user experience quality.

### Decision
Implement **Session-Isolated State Management**:
- Each tmux session gets dedicated state directory
- Session preferences stored in `/sessions/{session_id}/`
- Independent caching per session
- Session lifecycle management (create/update/cleanup)

### Consequences

**Positive**:
- Multiple sessions can have different voice configurations
- Session isolation prevents state conflicts
- Better error isolation (problems in one session don't affect others)
- Easier debugging and troubleshooting
- Natural cleanup when sessions are destroyed

**Negative**:
- Increased storage requirements
- More complex state management logic
- Potential for orphaned session data
- Cache duplication across sessions

**Directory Structure**:
```
/home/user/.tmux/claude/sessions/
├── tmux_20250803_103045/
│   ├── state.json
│   ├── preferences.yaml
│   ├── cache/
│   └── metrics.jsonl
└── tmux_20250803_114521/
    ├── state.json
    └── ...
```

### Alternatives Considered

1. **Global Shared State**
   - Pros: Simpler implementation, shared caching benefits
   - Cons: Session conflicts, difficulty in per-session customization

2. **Database-Based State**
   - Pros: ACID properties, complex queries
   - Cons: External dependency, overkill for file-based data

3. **Hybrid Global/Session Model**
   - Pros: Best of both approaches
   - Cons: Increased complexity, unclear precedence rules

---

## ADR-0009: Graceful Degradation Strategy

**Status**: Accepted  
**Date**: 2025-08-03  
**Deciders**: Reliability Team

### Context
The system operates in diverse environments where:
- Audio systems may not be available
- Network connectivity may be limited
- Required commands may not be installed
- Permissions may be restricted

The system should continue to function (possibly with reduced capability) rather than failing entirely.

### Decision
Implement **Graceful Degradation Strategy** with multiple fallback levels:
1. **Full Feature**: Voice synthesis + sound effects + AI integration
2. **Audio Only**: Sound effects without voice synthesis
3. **Visual Only**: Status bar notifications only
4. **Logging Only**: Silent operation with detailed logging

### Consequences

**Positive**:
- System remains functional in constrained environments
- Reduces user frustration from complete failures
- Better enterprise deployment compatibility
- Easier troubleshooting (clear degradation levels)

**Negative**:
- Complex fallback logic throughout codebase
- Testing requires multiple environment configurations
- Users may not be aware of degraded functionality
- Support complexity increases

**Fallback Logic**:
```bash
execute_voice_notification() {
    local text="$1"
    
    # Level 1: Full voice synthesis
    if universal_speak "$text"; then
        return 0
    fi
    
    # Level 2: Sound effects only
    if universal_play_sound "info"; then
        log_info "Fallback to sound effects: $text"
        return 0
    fi
    
    # Level 3: Visual notification
    if tmux display-message "$text"; then
        log_info "Fallback to visual: $text"
        return 0
    fi
    
    # Level 4: Logging only
    log_info "Silent mode: $text"
    return 0
}
```

### Alternatives Considered

1. **Fail-Fast Strategy**
   - Pros: Clear error states, simpler logic
   - Cons: Poor user experience, deployment difficulties

2. **Configuration-Driven Fallbacks**
   - Pros: User control over degradation
   - Cons: Complex configuration, user burden

3. **Auto-Repair Attempts**
   - Pros: System attempts to fix issues
   - Cons: Unpredictable behavior, potential security risks

---

## Decision History

| ADR | Title | Status | Date | Impact |
|-----|-------|--------|------|--------|
| 0001 | Shell Script Implementation | Accepted | 2025-08-03 | High |
| 0002 | Platform Abstraction Layer | Accepted | 2025-08-03 | High |
| 0003 | YAML Configuration System | Accepted | 2025-08-03 | High |
| 0004 | Equal Power Pan Law | Accepted | 2025-08-03 | Medium |
| 0005 | File-Based Caching | Accepted | 2025-08-03 | Medium |
| 0006 | PowerShell WSL Interop | Accepted | 2025-08-03 | High |
| 0007 | Three-State Status Detection | Accepted | 2025-08-03 | Medium |
| 0008 | Session-Isolated State | Accepted | 2025-08-03 | High |
| 0009 | Graceful Degradation | Accepted | 2025-08-03 | High |

## Future ADRs to Consider

### Pending Decisions
- **ADR-0010**: Metrics Collection and Analytics Strategy
- **ADR-0011**: Plugin Architecture for Extensions
- **ADR-0012**: Internationalization and Localization Approach
- **ADR-0013**: Cloud Integration for Configuration Sync
- **ADR-0014**: Container/Docker Deployment Strategy

### Review Schedule
- **Quarterly Reviews**: Every 3 months, review all accepted ADRs for continued relevance
- **Major Version Reviews**: Before major releases, ensure ADRs align with new features
- **Incident-Triggered Reviews**: After major incidents, review related architectural decisions

## ADR Maintenance Guidelines

### Creating New ADRs
1. Use sequential numbering (ADR-XXXX)
2. Follow the template structure strictly
3. Include implementation examples where relevant
4. Document all alternatives considered
5. Get review from relevant stakeholders before acceptance

### Updating Existing ADRs
- **Never modify accepted ADRs** - create superseding ADRs instead
- Mark superseded ADRs with clear references to replacements
- Maintain decision history for auditing purposes

### ADR Quality Criteria
- **Clarity**: Decision and rationale are clear
- **Completeness**: All alternatives and consequences documented
- **Traceability**: Links to relevant discussions and documentation
- **Implementability**: Sufficient detail for implementation teams