from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, List, Tuple

from . import hw1, hw2, midterm

SubId = str
ScoreNote = Tuple[int, str]
ScoresDict = Dict[SubId, ScoreNote]
Entry = Dict[str, object]


@dataclass(frozen=True)
class AssignmentSpec:
    name: str
    subs: List[Tuple[SubId, str, str]]
    entries: List[Entry]
    output: Path
    previous_policy: Callable[[SubId, int, str], int]


ASSIGNMENTS: List[AssignmentSpec] = [
    AssignmentSpec(
        name="midterm",
        subs=midterm.SUBS,
        entries=midterm.ENTRIES,
        output=Path("midterm/채점결과_전체.csv"),
        previous_policy=midterm.previous_subscore,
    ),
    AssignmentSpec(
        name="hw1",
        subs=hw1.SUBS,
        entries=hw1.ENTRIES,
        output=Path("correct/hw1/채점결과_전체.csv"),
        previous_policy=hw1.previous_subscore,
    ),
    AssignmentSpec(
        name="hw2",
        subs=hw2.SUBS,
        entries=hw2.ENTRIES,
        output=Path("correct/hw2/채점결과_전체.csv"),
        previous_policy=hw2.previous_subscore,
    ),
]
__all__ = ['ASSIGNMENTS', 'AssignmentSpec']
