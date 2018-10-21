#!/usr/bin/python3

def num_coin_combos(amount, coin_list):
    head_coin, *tail_coins = coin_list
    count = 0 if amount % head_coin else 1
    if tail_coins:
        while amount > 0:
            count += num_coin_combos(amount, tail_coins)
            amount -= head_coin
    return count

if __name__ == "__main__":
    amount = 200
    coin_list = [200, 100, 50, 20, 10, 5, 2, 1]
    print(num_coin_combos(amount, coin_list))
