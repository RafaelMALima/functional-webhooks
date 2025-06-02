{-# LINE 1 "Database/SQLite3/Bindings/Types.hsc" #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.SQLite3.Bindings.Types (
    -- * Objects
    -- | <https://www.sqlite.org/c3ref/objlist.html>
    CDatabase,
    CStatement,
    CValue,
    CContext,
    CBlob,
    CBackup,

    -- * Enumerations

    -- ** Error
    CError(..),
    decodeError,
    encodeError,
    Error(..),

    -- ** ColumnType
    CColumnType(..),
    decodeColumnType,
    encodeColumnType,
    ColumnType(..),

    -- * Indices
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,

    -- ** Indices (FFI)
    CParamIndex(..),
    CColumnIndex(..),
    CColumnCount,

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_STATIC,
    c_SQLITE_TRANSIENT,
    c_SQLITE_UTF8,

    -- * Custom functions
    ArgCount(..),
    ArgIndex,
    CArgCount(..),
    c_SQLITE_DETERMINISTIC,

    -- * Conversion to and from FFI types
    FFIType(..),
) where


{-# LINE 60 "Database/SQLite3/Bindings/Types.hsc" #-}


{-# LINE 62 "Database/SQLite3/Bindings/Types.hsc" #-}

import GHC.Generics
import Foreign.C.Types
import Foreign.Ptr

-- Result code documentation copied from <https://www.sqlite.org/c3ref/c_abort.html>

-- | <https://www.sqlite.org/c3ref/c_abort.html>
-- <https://www.sqlite.org/c3ref/c_abort_rollback.html>
data Error = ErrorOK                     -- ^ Successful result
           | ErrorError                  -- ^ SQL error or missing database
           | ErrorInternal               -- ^ Internal logic error in SQLite
           | ErrorPermission             -- ^ Access permission denied
           | ErrorAbort                  -- ^ Callback routine requested an abort
           | ErrorBusy                   -- ^ The database file is locked
           | ErrorLocked                 -- ^ A table in the database is locked
           | ErrorNoMemory               -- ^ A @malloc()@ failed
           | ErrorReadOnly               -- ^ Attempt to write a readonly database
           | ErrorInterrupt              -- ^ Operation terminated by @sqlite3_interrupt()@
           | ErrorIO                     -- ^ Some kind of disk I/O error occurred
           | ErrorCorrupt                -- ^ The database disk image is malformed
           | ErrorNotFound               -- ^ Unknown opcode in @sqlite3_file_control()@
           | ErrorFull                   -- ^ Insertion failed because database is full
           | ErrorCan'tOpen              -- ^ Unable to open the database file
           | ErrorProtocol               -- ^ Database lock protocol error
           | ErrorEmpty                  -- ^ Database is empty
           | ErrorSchema                 -- ^ The database schema changed
           | ErrorTooBig                 -- ^ String or BLOB exceeds size limit
           | ErrorConstraint             -- ^ Abort due to constraint violation
           | ErrorMismatch               -- ^ Data type mismatch
           | ErrorMisuse                 -- ^ Library used incorrectly
           | ErrorNoLargeFileSupport     -- ^ Uses OS features not supported on host
           | ErrorAuthorization          -- ^ Authorization denied
           | ErrorFormat                 -- ^ Auxiliary database format error
           | ErrorRange                  -- ^ 2nd parameter to sqlite3_bind out of range
           | ErrorNotADatabase           -- ^ File opened that is not a database file
           | ErrorNotice                 -- ^ Notifications from sqlite3_log()
           | ErrorWarning                -- ^ Warnings from sqlite3_log()
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing

           | ErrorErrorMissingCollatingSquence
           | ErrorErrorRetry
           | ErrorErrorSnapshot
           | ErrorIORead
           | ErrorIOShortRead
           | ErrorIOWrite
           | ErrorIOFsync
           | ErrorIODirectoryFsync
           | ErrorIOTruncate
           | ErrorIOFstat
           | ErrorIOUnlock
           | ErrorIOReadLock
           | ErrorIOBlocked
           | ErrorIODelete
           | ErrorIONoMemory
           | ErrorIOAccess
           | ErrorIOCheckReservedLock
           | ErrorIOLock
           | ErrorIOClose
           | ErrorIODirectoryClose
           | ErrorIOShmOpen
           | ErrorIOShmSize
           | ErrorIOShmLock
           | ErrorIOShmMap
           | ErrorIOSeek
           | ErrorIODeleteNoEntity
           | ErrorIOMmap
           | ErrorIOGetTempPath
           | ErrorIOConvertedPath
           | ErrorIOVNode
           | ErrorIOAuth
           | ErrorIOBeginAtomic
           | ErrorIOCommitAtomic
           | ErrorIORollbackAtomic
           | ErrorIOData
           | ErrorIOCorruptFilesystem
           | ErrorLockedSharedCache
           | ErrorLockedVirtualTable
           | ErrorBusyRecovery
           | ErrorBusySnapshot
           | ErrorBusyTimeout
           | ErrorCan'tOpenNotTempDirectory
           | ErrorCan'tOpenIsDirectory
           | ErrorCan'tOpenFullPath
           | ErrorCan'tOpenConvertedPath
           | ErrorCan'tOpenDirtyWriteAheadLog
           | ErrorCan'tOpenSymlink
           | ErrorCorruptVirtualTable
           | ErrorCorruptSequence
           | ErrorCorruptIndex
           | ErrorReadOnlyRecovery
           | ErrorReadOnlyCan'tLock
           | ErrorReadOnlyRollback
           | ErrorReadOnlyDatabaseMoved
           | ErrorReadOnlyCan'tInit
           | ErrorReadOnlyDirectory
           | ErrorAbortRollback
           | ErrorConstraintCheck
           | ErrorConstraintCommitHook
           | ErrorConstraintForeignKey
           | ErrorConstraintFunction
           | ErrorConstraintNotNull
           | ErrorConstraintPrimaryKey
           | ErrorConstraintTrigger
           | ErrorConstraintUnique
           | ErrorConstraintVirtualTable
           | ErrorConstraintRowId
           | ErrorConstraintPinned
           | ErrorConstraintDataType
           | ErrorNoticeRecoverWriteAheadLog
           | ErrorNoticeRecoverRollback
           | ErrorWarningAutoIndex
           | ErrorAuthUser
           | ErrorOkLoadPermanently
             deriving (Eq, Show, Generic)

-- | <https://www.sqlite.org/c3ref/c_blob.html>
data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

-- | <https://www.sqlite.org/c3ref/sqlite3.html>
--
-- @CDatabase@ = @sqlite3@
data CDatabase

-- | <https://www.sqlite.org/c3ref/stmt.html>
--
-- @CStatement@ = @sqlite3_stmt@
data CStatement

-- | <https://www.sqlite.org/c3ref/value.html>
--
-- @CValue@ = @sqlite3_value@
data CValue

-- | <https://www.sqlite.org/c3ref/context.html>
--
-- @CContext@ = @sqlite3_context@
data CContext

-- | <https://www.sqlite.org/c3ref/blob.html>
--
-- @CBlob@ = @sqlite3_blob@
data CBlob

-- | <https://www.sqlite.org/c3ref/backup.html>
--
-- @CBackup@ = @sqlite3_backup@
data CBackup

-- | Index of a parameter in a parameterized query.
-- Parameter indices start from 1.
--
-- When a query is 'Database.SQLite3.prepare'd, SQLite allocates an
-- array indexed from 1 to the highest parameter index.  For example:
--
-- >>Right stmt <- prepare conn "SELECT ?1, ?5, ?3, ?"
-- >>bindParameterCount stmt
-- >ParamIndex 6
--
-- This will allocate an array indexed from 1 to 6 (@?@ takes the highest
-- preceding index plus one).  The array is initialized with null values.
-- When you bind a parameter with 'Database.SQLite3.bindSQLData', it assigns a
-- new value to one of these indices.
--
-- See <https://www.sqlite.org/lang_expr.html#varparam> for the syntax of
-- parameter placeholders, and how parameter indices are assigned.
newtype ParamIndex = ParamIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ParamIndex where
    show (ParamIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ParamIndex where
    minBound = ParamIndex (fromIntegral (minBound :: CInt))
    maxBound = ParamIndex (fromIntegral (maxBound :: CInt))

-- | Index of a column in a result set.  Column indices start from 0.
newtype ColumnIndex = ColumnIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ColumnIndex where
    show (ColumnIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ColumnIndex where
    minBound = ColumnIndex (fromIntegral (minBound :: CInt))
    maxBound = ColumnIndex (fromIntegral (maxBound :: CInt))

-- | Number of columns in a result set.
type ColumnCount = ColumnIndex

newtype CParamIndex = CParamIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CParamIndex where
    show (CParamIndex n) = show n

newtype CColumnIndex = CColumnIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CColumnIndex where
    show (CColumnIndex n) = show n

type CColumnCount = CColumnIndex

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | <https://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

-- | Tells SQLite3 that the content pointer is constant and will never change
c_SQLITE_STATIC :: Ptr CDestructor
c_SQLITE_STATIC = intPtrToPtr 0

-- | Tells SQLite3 to make its own private copy of the data
c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)

c_SQLITE_UTF8 :: CInt
c_SQLITE_UTF8 = 1
{-# LINE 296 "Database/SQLite3/Bindings/Types.hsc" #-}

-- | Number of arguments of a user defined SQL function.
newtype ArgCount = ArgCount Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ArgCount where
    show (ArgCount n) = show n

instance Bounded ArgCount where
    minBound = ArgCount 0
    maxBound = ArgCount (6)
{-# LINE 308 "Database/SQLite3/Bindings/Types.hsc" #-}

-- | Index of an argument to a custom function. Indices start from 0.
type ArgIndex = ArgCount

newtype CArgCount = CArgCount CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CArgCount where
    show (CArgCount n) = show n

instance Bounded CArgCount where
    minBound = CArgCount (-1)
    maxBound = CArgCount 6
{-# LINE 322 "Database/SQLite3/Bindings/Types.hsc" #-}

-- | Tells SQLite3 that the defined custom SQL function is deterministic.
c_SQLITE_DETERMINISTIC :: CInt
c_SQLITE_DETERMINISTIC = 2048
{-# LINE 326 "Database/SQLite3/Bindings/Types.hsc" #-}

-- | <https://www.sqlite.org/c3ref/c_abort.html>
newtype CError = CError CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.  If the error code is invalid, or
-- perhaps introduced in a newer version of SQLite but this library has not
-- been updated to support it, the result is undefined.
--
-- To be clear, if 'decodeError' fails, it is /undefined behavior/, not an
-- exception you can handle.
--
-- Therefore, do not use direct-sqlite with a different version of SQLite than
-- the one bundled (currently, 3.24.0).  If you do, ensure that 'decodeError'
-- and 'decodeColumnType' are still exhaustive.
decodeError :: CError -> Error
decodeError (CError n) = case n of
    0         -> ErrorOK
{-# LINE 344 "Database/SQLite3/Bindings/Types.hsc" #-}
    1      -> ErrorError
{-# LINE 345 "Database/SQLite3/Bindings/Types.hsc" #-}
    2   -> ErrorInternal
{-# LINE 346 "Database/SQLite3/Bindings/Types.hsc" #-}
    3       -> ErrorPermission
{-# LINE 347 "Database/SQLite3/Bindings/Types.hsc" #-}
    4      -> ErrorAbort
{-# LINE 348 "Database/SQLite3/Bindings/Types.hsc" #-}
    5       -> ErrorBusy
{-# LINE 349 "Database/SQLite3/Bindings/Types.hsc" #-}
    6     -> ErrorLocked
{-# LINE 350 "Database/SQLite3/Bindings/Types.hsc" #-}
    7      -> ErrorNoMemory
{-# LINE 351 "Database/SQLite3/Bindings/Types.hsc" #-}
    8   -> ErrorReadOnly
{-# LINE 352 "Database/SQLite3/Bindings/Types.hsc" #-}
    9  -> ErrorInterrupt
{-# LINE 353 "Database/SQLite3/Bindings/Types.hsc" #-}
    10      -> ErrorIO
{-# LINE 354 "Database/SQLite3/Bindings/Types.hsc" #-}
    11    -> ErrorCorrupt
{-# LINE 355 "Database/SQLite3/Bindings/Types.hsc" #-}
    12   -> ErrorNotFound
{-# LINE 356 "Database/SQLite3/Bindings/Types.hsc" #-}
    13       -> ErrorFull
{-# LINE 357 "Database/SQLite3/Bindings/Types.hsc" #-}
    14   -> ErrorCan'tOpen
{-# LINE 358 "Database/SQLite3/Bindings/Types.hsc" #-}
    15   -> ErrorProtocol
{-# LINE 359 "Database/SQLite3/Bindings/Types.hsc" #-}
    16      -> ErrorEmpty
{-# LINE 360 "Database/SQLite3/Bindings/Types.hsc" #-}
    17     -> ErrorSchema
{-# LINE 361 "Database/SQLite3/Bindings/Types.hsc" #-}
    18     -> ErrorTooBig
{-# LINE 362 "Database/SQLite3/Bindings/Types.hsc" #-}
    19 -> ErrorConstraint
{-# LINE 363 "Database/SQLite3/Bindings/Types.hsc" #-}
    20   -> ErrorMismatch
{-# LINE 364 "Database/SQLite3/Bindings/Types.hsc" #-}
    21     -> ErrorMisuse
{-# LINE 365 "Database/SQLite3/Bindings/Types.hsc" #-}
    22      -> ErrorNoLargeFileSupport
{-# LINE 366 "Database/SQLite3/Bindings/Types.hsc" #-}
    23       -> ErrorAuthorization
{-# LINE 367 "Database/SQLite3/Bindings/Types.hsc" #-}
    24     -> ErrorFormat
{-# LINE 368 "Database/SQLite3/Bindings/Types.hsc" #-}
    25      -> ErrorRange
{-# LINE 369 "Database/SQLite3/Bindings/Types.hsc" #-}
    26     -> ErrorNotADatabase
{-# LINE 370 "Database/SQLite3/Bindings/Types.hsc" #-}
    27     -> ErrorNotice
{-# LINE 371 "Database/SQLite3/Bindings/Types.hsc" #-}
    28    -> ErrorWarning
{-# LINE 372 "Database/SQLite3/Bindings/Types.hsc" #-}
    100        -> ErrorRow
{-# LINE 373 "Database/SQLite3/Bindings/Types.hsc" #-}
    101       -> ErrorDone
{-# LINE 374 "Database/SQLite3/Bindings/Types.hsc" #-}
    -- extended result codes
    257   -> ErrorErrorMissingCollatingSquence
{-# LINE 376 "Database/SQLite3/Bindings/Types.hsc" #-}
    513             -> ErrorErrorRetry
{-# LINE 377 "Database/SQLite3/Bindings/Types.hsc" #-}
    769          -> ErrorErrorSnapshot
{-# LINE 378 "Database/SQLite3/Bindings/Types.hsc" #-}
    266              -> ErrorIORead
{-# LINE 379 "Database/SQLite3/Bindings/Types.hsc" #-}
    522        -> ErrorIOShortRead
{-# LINE 380 "Database/SQLite3/Bindings/Types.hsc" #-}
    778             -> ErrorIOWrite
{-# LINE 381 "Database/SQLite3/Bindings/Types.hsc" #-}
    1034             -> ErrorIOFsync
{-# LINE 382 "Database/SQLite3/Bindings/Types.hsc" #-}
    1290         -> ErrorIODirectoryFsync
{-# LINE 383 "Database/SQLite3/Bindings/Types.hsc" #-}
    1546          -> ErrorIOTruncate
{-# LINE 384 "Database/SQLite3/Bindings/Types.hsc" #-}
    1802             -> ErrorIOFstat
{-# LINE 385 "Database/SQLite3/Bindings/Types.hsc" #-}
    2058            -> ErrorIOUnlock
{-# LINE 386 "Database/SQLite3/Bindings/Types.hsc" #-}
    2314            -> ErrorIOReadLock
{-# LINE 387 "Database/SQLite3/Bindings/Types.hsc" #-}
    2570            -> ErrorIODelete
{-# LINE 388 "Database/SQLite3/Bindings/Types.hsc" #-}
    2826           -> ErrorIOBlocked
{-# LINE 389 "Database/SQLite3/Bindings/Types.hsc" #-}
    3082             -> ErrorIONoMemory
{-# LINE 390 "Database/SQLite3/Bindings/Types.hsc" #-}
    3338            -> ErrorIOAccess
{-# LINE 391 "Database/SQLite3/Bindings/Types.hsc" #-}
    3594 -> ErrorIOCheckReservedLock
{-# LINE 392 "Database/SQLite3/Bindings/Types.hsc" #-}
    3850              -> ErrorIOLock
{-# LINE 393 "Database/SQLite3/Bindings/Types.hsc" #-}
    4106             -> ErrorIOClose
{-# LINE 394 "Database/SQLite3/Bindings/Types.hsc" #-}
    4362         -> ErrorIODirectoryClose
{-# LINE 395 "Database/SQLite3/Bindings/Types.hsc" #-}
    4618           -> ErrorIOShmOpen
{-# LINE 396 "Database/SQLite3/Bindings/Types.hsc" #-}
    4874           -> ErrorIOShmSize
{-# LINE 397 "Database/SQLite3/Bindings/Types.hsc" #-}
    5130           -> ErrorIOShmLock
{-# LINE 398 "Database/SQLite3/Bindings/Types.hsc" #-}
    5386            -> ErrorIOShmMap
{-# LINE 399 "Database/SQLite3/Bindings/Types.hsc" #-}
    5642              -> ErrorIOSeek
{-# LINE 400 "Database/SQLite3/Bindings/Types.hsc" #-}
    5898      -> ErrorIODeleteNoEntity
{-# LINE 401 "Database/SQLite3/Bindings/Types.hsc" #-}
    6154              -> ErrorIOMmap
{-# LINE 402 "Database/SQLite3/Bindings/Types.hsc" #-}
    6410       -> ErrorIOGetTempPath
{-# LINE 403 "Database/SQLite3/Bindings/Types.hsc" #-}
    6666          -> ErrorIOConvertedPath
{-# LINE 404 "Database/SQLite3/Bindings/Types.hsc" #-}
    6922             -> ErrorIOVNode
{-# LINE 405 "Database/SQLite3/Bindings/Types.hsc" #-}
    7178              -> ErrorIOAuth
{-# LINE 406 "Database/SQLite3/Bindings/Types.hsc" #-}
    7434      -> ErrorIOBeginAtomic
{-# LINE 407 "Database/SQLite3/Bindings/Types.hsc" #-}
    7690     -> ErrorIOCommitAtomic
{-# LINE 408 "Database/SQLite3/Bindings/Types.hsc" #-}
    7946   -> ErrorIORollbackAtomic
{-# LINE 409 "Database/SQLite3/Bindings/Types.hsc" #-}
    8202              -> ErrorIOData
{-# LINE 410 "Database/SQLite3/Bindings/Types.hsc" #-}
    8458         -> ErrorIOCorruptFilesystem
{-# LINE 411 "Database/SQLite3/Bindings/Types.hsc" #-}
    262      -> ErrorLockedSharedCache
{-# LINE 412 "Database/SQLite3/Bindings/Types.hsc" #-}
    518             -> ErrorLockedVirtualTable
{-# LINE 413 "Database/SQLite3/Bindings/Types.hsc" #-}
    261           -> ErrorBusyRecovery
{-# LINE 414 "Database/SQLite3/Bindings/Types.hsc" #-}
    517           -> ErrorBusySnapshot
{-# LINE 415 "Database/SQLite3/Bindings/Types.hsc" #-}
    773            -> ErrorBusyTimeout
{-# LINE 416 "Database/SQLite3/Bindings/Types.hsc" #-}
    270      -> ErrorCan'tOpenNotTempDirectory
{-# LINE 417 "Database/SQLite3/Bindings/Types.hsc" #-}
    526          -> ErrorCan'tOpenIsDirectory
{-# LINE 418 "Database/SQLite3/Bindings/Types.hsc" #-}
    782       -> ErrorCan'tOpenFullPath
{-# LINE 419 "Database/SQLite3/Bindings/Types.hsc" #-}
    1038       -> ErrorCan'tOpenConvertedPath
{-# LINE 420 "Database/SQLite3/Bindings/Types.hsc" #-}
    1294       -> ErrorCan'tOpenDirtyWriteAheadLog
{-# LINE 421 "Database/SQLite3/Bindings/Types.hsc" #-}
    1550        -> ErrorCan'tOpenSymlink
{-# LINE 422 "Database/SQLite3/Bindings/Types.hsc" #-}
    267            -> ErrorCorruptVirtualTable
{-# LINE 423 "Database/SQLite3/Bindings/Types.hsc" #-}
    523        -> ErrorCorruptSequence
{-# LINE 424 "Database/SQLite3/Bindings/Types.hsc" #-}
    779           -> ErrorCorruptIndex
{-# LINE 425 "Database/SQLite3/Bindings/Types.hsc" #-}
    264       -> ErrorReadOnlyRecovery
{-# LINE 426 "Database/SQLite3/Bindings/Types.hsc" #-}
    520       -> ErrorReadOnlyCan'tLock
{-# LINE 427 "Database/SQLite3/Bindings/Types.hsc" #-}
    776       -> ErrorReadOnlyRollback
{-# LINE 428 "Database/SQLite3/Bindings/Types.hsc" #-}
    1032        -> ErrorReadOnlyDatabaseMoved
{-# LINE 429 "Database/SQLite3/Bindings/Types.hsc" #-}
    1288       -> ErrorReadOnlyCan'tInit
{-# LINE 430 "Database/SQLite3/Bindings/Types.hsc" #-}
    1544      -> ErrorReadOnlyDirectory
{-# LINE 431 "Database/SQLite3/Bindings/Types.hsc" #-}
    516          -> ErrorAbortRollback
{-# LINE 432 "Database/SQLite3/Bindings/Types.hsc" #-}
    275        -> ErrorConstraintCheck
{-# LINE 433 "Database/SQLite3/Bindings/Types.hsc" #-}
    531   -> ErrorConstraintCommitHook
{-# LINE 434 "Database/SQLite3/Bindings/Types.hsc" #-}
    787   -> ErrorConstraintForeignKey
{-# LINE 435 "Database/SQLite3/Bindings/Types.hsc" #-}
    1043     -> ErrorConstraintFunction
{-# LINE 436 "Database/SQLite3/Bindings/Types.hsc" #-}
    1299      -> ErrorConstraintNotNull
{-# LINE 437 "Database/SQLite3/Bindings/Types.hsc" #-}
    1555   -> ErrorConstraintPrimaryKey
{-# LINE 438 "Database/SQLite3/Bindings/Types.hsc" #-}
    1811      -> ErrorConstraintTrigger
{-# LINE 439 "Database/SQLite3/Bindings/Types.hsc" #-}
    2067       -> ErrorConstraintUnique
{-# LINE 440 "Database/SQLite3/Bindings/Types.hsc" #-}
    2323         -> ErrorConstraintVirtualTable
{-# LINE 441 "Database/SQLite3/Bindings/Types.hsc" #-}
    2579        -> ErrorConstraintRowId
{-# LINE 442 "Database/SQLite3/Bindings/Types.hsc" #-}
    2835       -> ErrorConstraintPinned
{-# LINE 443 "Database/SQLite3/Bindings/Types.hsc" #-}
    3091     -> ErrorConstraintDataType
{-# LINE 444 "Database/SQLite3/Bindings/Types.hsc" #-}
    283      -> ErrorNoticeRecoverWriteAheadLog
{-# LINE 445 "Database/SQLite3/Bindings/Types.hsc" #-}
    539 -> ErrorNoticeRecoverRollback
{-# LINE 446 "Database/SQLite3/Bindings/Types.hsc" #-}
    284       -> ErrorWarningAutoIndex
{-# LINE 447 "Database/SQLite3/Bindings/Types.hsc" #-}
    279               -> ErrorAuthUser
{-# LINE 448 "Database/SQLite3/Bindings/Types.hsc" #-}
    256     -> ErrorOkLoadPermanently
{-# LINE 449 "Database/SQLite3/Bindings/Types.hsc" #-}
    _                          -> error $ "decodeError " ++ show n

encodeError :: Error -> CError
encodeError err = CError $ case err of
    ErrorOK                 -> 0
{-# LINE 454 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorError              -> 1
{-# LINE 455 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorInternal           -> 2
{-# LINE 456 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorPermission         -> 3
{-# LINE 457 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorAbort              -> 4
{-# LINE 458 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorBusy               -> 5
{-# LINE 459 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorLocked             -> 6
{-# LINE 460 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNoMemory           -> 7
{-# LINE 461 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnly           -> 8
{-# LINE 462 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorInterrupt          -> 9
{-# LINE 463 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIO                 -> 10
{-# LINE 464 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCorrupt            -> 11
{-# LINE 465 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNotFound           -> 12
{-# LINE 466 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorFull               -> 13
{-# LINE 467 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpen          -> 14
{-# LINE 468 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorProtocol           -> 15
{-# LINE 469 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorEmpty              -> 16
{-# LINE 470 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorSchema             -> 17
{-# LINE 471 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorTooBig             -> 18
{-# LINE 472 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraint         -> 19
{-# LINE 473 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorMismatch           -> 20
{-# LINE 474 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorMisuse             -> 21
{-# LINE 475 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNoLargeFileSupport -> 22
{-# LINE 476 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorAuthorization      -> 23
{-# LINE 477 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorFormat             -> 24
{-# LINE 478 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorRange              -> 25
{-# LINE 479 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNotADatabase       -> 26
{-# LINE 480 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNotice             -> 27
{-# LINE 481 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorWarning            -> 28
{-# LINE 482 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorRow                -> 100
{-# LINE 483 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorDone               -> 101
{-# LINE 484 "Database/SQLite3/Bindings/Types.hsc" #-}

    ErrorErrorMissingCollatingSquence  -> 257
{-# LINE 486 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorErrorRetry                    -> 513
{-# LINE 487 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorErrorSnapshot                 -> 769
{-# LINE 488 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIORead                        -> 266
{-# LINE 489 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOShortRead                   -> 522
{-# LINE 490 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOWrite                       -> 778
{-# LINE 491 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOFsync                       -> 1034
{-# LINE 492 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIODirectoryFsync              -> 1290
{-# LINE 493 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOTruncate                    -> 1546
{-# LINE 494 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOFstat                       -> 1802
{-# LINE 495 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOUnlock                      -> 2058
{-# LINE 496 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOReadLock                    -> 2314
{-# LINE 497 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOBlocked                     -> 2826
{-# LINE 498 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIODelete                      -> 2570
{-# LINE 499 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIONoMemory                    -> 3082
{-# LINE 500 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOAccess                      -> 3338
{-# LINE 501 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOCheckReservedLock           -> 3594
{-# LINE 502 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOLock                        -> 3850
{-# LINE 503 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOClose                       -> 4106
{-# LINE 504 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIODirectoryClose              -> 4362
{-# LINE 505 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOShmOpen                     -> 4618
{-# LINE 506 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOShmSize                     -> 4874
{-# LINE 507 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOShmLock                     -> 5130
{-# LINE 508 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOShmMap                      -> 5386
{-# LINE 509 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOSeek                        -> 5642
{-# LINE 510 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIODeleteNoEntity              -> 5898
{-# LINE 511 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOMmap                        -> 6154
{-# LINE 512 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOGetTempPath                 -> 6410
{-# LINE 513 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOConvertedPath               -> 6666
{-# LINE 514 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOVNode                       -> 6922
{-# LINE 515 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOAuth                        -> 7178
{-# LINE 516 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOBeginAtomic                 -> 7434
{-# LINE 517 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOCommitAtomic                -> 7690
{-# LINE 518 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIORollbackAtomic              -> 7946
{-# LINE 519 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOData                        -> 8202
{-# LINE 520 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorIOCorruptFilesystem           -> 8458
{-# LINE 521 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorLockedSharedCache             -> 262
{-# LINE 522 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorLockedVirtualTable            -> 518
{-# LINE 523 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorBusyRecovery                  -> 261
{-# LINE 524 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorBusySnapshot                  -> 517
{-# LINE 525 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorBusyTimeout                   -> 773
{-# LINE 526 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenNotTempDirectory     -> 270
{-# LINE 527 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenIsDirectory          -> 526
{-# LINE 528 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenFullPath             -> 782
{-# LINE 529 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenConvertedPath        -> 1038
{-# LINE 530 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenDirtyWriteAheadLog   -> 1294
{-# LINE 531 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCan'tOpenSymlink              -> 1550
{-# LINE 532 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCorruptVirtualTable           -> 267
{-# LINE 533 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCorruptSequence               -> 523
{-# LINE 534 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorCorruptIndex                  -> 779
{-# LINE 535 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyRecovery              -> 264
{-# LINE 536 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyCan'tLock             -> 520
{-# LINE 537 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyRollback              -> 776
{-# LINE 538 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyDatabaseMoved         -> 1032
{-# LINE 539 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyCan'tInit             -> 1288
{-# LINE 540 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorReadOnlyDirectory             -> 1544
{-# LINE 541 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorAbortRollback                 -> 516
{-# LINE 542 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintCheck               -> 275
{-# LINE 543 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintCommitHook          -> 531
{-# LINE 544 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintForeignKey          -> 787
{-# LINE 545 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintFunction            -> 1043
{-# LINE 546 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintNotNull             -> 1299
{-# LINE 547 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintPrimaryKey          -> 1555
{-# LINE 548 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintTrigger             -> 1811
{-# LINE 549 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintUnique              -> 2067
{-# LINE 550 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintVirtualTable        -> 2323
{-# LINE 551 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintRowId               -> 2579
{-# LINE 552 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintPinned              -> 2835
{-# LINE 553 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorConstraintDataType            -> 3091
{-# LINE 554 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNoticeRecoverWriteAheadLog    -> 283
{-# LINE 555 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorNoticeRecoverRollback         -> 539
{-# LINE 556 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorWarningAutoIndex              -> 284
{-# LINE 557 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorAuthUser                      -> 279
{-# LINE 558 "Database/SQLite3/Bindings/Types.hsc" #-}
    ErrorOkLoadPermanently             -> 256
{-# LINE 559 "Database/SQLite3/Bindings/Types.hsc" #-}

-- | <https://www.sqlite.org/c3ref/c_blob.html>
newtype CColumnType = CColumnType CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.
-- See 'decodeError' for more information.
decodeColumnType :: CColumnType -> ColumnType
decodeColumnType (CColumnType n) = case n of
    1 -> IntegerColumn
{-# LINE 569 "Database/SQLite3/Bindings/Types.hsc" #-}
    2   -> FloatColumn
{-# LINE 570 "Database/SQLite3/Bindings/Types.hsc" #-}
    3    -> TextColumn
{-# LINE 571 "Database/SQLite3/Bindings/Types.hsc" #-}
    4    -> BlobColumn
{-# LINE 572 "Database/SQLite3/Bindings/Types.hsc" #-}
    5    -> NullColumn
{-# LINE 573 "Database/SQLite3/Bindings/Types.hsc" #-}
    _                       -> error $ "decodeColumnType " ++ show n

encodeColumnType :: ColumnType -> CColumnType
encodeColumnType t = CColumnType $ case t of
    IntegerColumn -> 1
{-# LINE 578 "Database/SQLite3/Bindings/Types.hsc" #-}
    FloatColumn   -> 2
{-# LINE 579 "Database/SQLite3/Bindings/Types.hsc" #-}
    TextColumn    -> 3
{-# LINE 580 "Database/SQLite3/Bindings/Types.hsc" #-}
    BlobColumn    -> 4
{-# LINE 581 "Database/SQLite3/Bindings/Types.hsc" #-}
    NullColumn    -> 5
{-# LINE 582 "Database/SQLite3/Bindings/Types.hsc" #-}

------------------------------------------------------------------------
-- Conversion to and from FFI types

-- | The "Database.SQLite3" and "Database.SQLite3.Direct" modules use
-- higher-level representations of some types than those used in the
-- FFI signatures ("Database.SQLite3.Bindings").  This typeclass
-- helps with the conversions.
class FFIType public ffi | public -> ffi, ffi -> public where
    toFFI   :: public -> ffi
    fromFFI :: ffi -> public

instance FFIType ParamIndex CParamIndex where
    toFFI (ParamIndex n) = CParamIndex (fromIntegral n)
    fromFFI (CParamIndex n) = ParamIndex (fromIntegral n)

instance FFIType ColumnIndex CColumnIndex where
    toFFI (ColumnIndex n) = CColumnIndex (fromIntegral n)
    fromFFI (CColumnIndex n) = ColumnIndex (fromIntegral n)

instance FFIType Error CError where
    toFFI = encodeError
    fromFFI = decodeError

instance FFIType ColumnType CColumnType where
    toFFI = encodeColumnType
    fromFFI = decodeColumnType

instance FFIType ArgCount CArgCount where
    toFFI (ArgCount n)  = CArgCount (fromIntegral n)
    fromFFI (CArgCount n) = ArgCount (fromIntegral n)
