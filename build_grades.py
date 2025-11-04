import csv
from collections import OrderedDict
from typing import Iterable, List, Tuple

from assignments import ASSIGNMENTS, AssignmentSpec


SubField = Tuple[str, str, str]


def build_fieldnames(subs: Iterable[SubField]) -> List[str]:
    fieldnames: List[str] = ["student"]
    for _, score_field, note_field in subs:
        fieldnames.extend([score_field, note_field])
    fieldnames.extend(["previous_total", "total", "total_change"])
    return fieldnames


def write_assignment(spec: AssignmentSpec) -> None:
    spec.output.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = build_fieldnames(spec.subs)
    changed: List[Tuple[str, int, int]] = []

    with spec.output.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, quoting=csv.QUOTE_ALL)
        writer.writeheader()

        for entry in spec.entries:
            row = OrderedDict()
            row["student"] = entry["student"]
            previous_total = 0
            total = 0

            for sub_id, score_field, note_field in spec.subs:
                score, note = entry["scores"][sub_id]
                row[score_field] = score
                row[note_field] = note
                total += score
                previous_total += spec.previous_policy(sub_id, score, note)

            row["previous_total"] = previous_total
            row["total"] = total
            delta = total - previous_total
            row["total_change"] = delta

            if delta != 0:
                changed.append((entry["student"], previous_total, total))

            writer.writerow(row)

    if not spec.entries:
        print(f"[{spec.name}] 채점 대상 없음")
    elif changed:
        print(f"[{spec.name}] 점수가 변경된 학생:")
        for student, old_total, new_total in changed:
            print(f"- {student}: {old_total} -> {new_total}")
    else:
        print(f"[{spec.name}] 점수 변동 없음")


for assignment in ASSIGNMENTS:
    write_assignment(assignment)
