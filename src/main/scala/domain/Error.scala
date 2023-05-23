package chess

trait Error

case object MoveValidationError extends Error

case object MoveCausesCheck extends Error