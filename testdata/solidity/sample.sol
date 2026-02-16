// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";

contract SimpleToken {
    mapping(address => uint256) private _balances;
    uint256 private _totalSupply;
    
    event Transfer(address indexed from, address indexed to, uint256 value);
    
    modifier onlyPositive(uint256 amount) {
        require(amount > 0, "Amount must be positive");
        _;
    }
    
    function transfer(address to, uint256 amount) public onlyPositive(amount) returns (bool) {
        _balances[msg.sender] -= amount;
        _balances[to] += amount;
        emit Transfer(msg.sender, to, amount);
        return true;
    }
    
    function balanceOf(address account) public view returns (uint256) {
        return _balances[account];
    }
}
