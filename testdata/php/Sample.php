<?php

declare(strict_types=1);

namespace App\Controllers;

use Vendor\Package\SomeClass;
use Vendor\Package\AnotherClass as Alias;
use Vendor\Package\{Foo, Bar, Baz};
use function Vendor\Helpers\helper_func;
use const Vendor\Config\MAX_RETRIES;

/**
 * This is a block comment that should be skipped.
 */
abstract class UserController extends BaseController implements Loggable, Serializable
{
    public const STATUS_ACTIVE = 'active';
    protected const STATUS_INACTIVE = 'inactive';
    private const SECRET_KEY = 'abc123';

    public string $name;
    protected static int $count = 0;
    private readonly ?string $email;

    public function __construct(
        private readonly string $id,
        string $name,
        ?string $email = null
    ) {
        $this->name = $name;
        $this->email = $email;
    }

    public function getName(): string
    {
        return $this->name;
    }

    protected static function calculate(int $a, int $b): int
    {
        return $a + $b;
    }

    private function secretMethod(): void
    {
        // do nothing
    }

    public function process(array $items, ?callable $callback = null): array
    {
        return array_map($callback, $items);
    }
}

interface Configurable
{
    public function configure(array $options): void;
}

trait Cacheable
{
    public function cache(string $key, mixed $value): void
    {
        // cache implementation
    }
}

enum Status: string
{
    case Active = 'active';
    case Inactive = 'inactive';
}

final class AdminController extends UserController
{
    public function promote(string $userId): bool
    {
        return true;
    }
}

function standalone_helper(string $input): string
{
    return strtoupper($input);
}
